/*
 * player.c++	-   Standard MIDI File Player Interface
 */

#include "MainWindow.h"
#include "Player.h"

#include <sys/prctl.h>
#include <libc.h>
#include <sys/stropts.h>
#include <sys/poll.h>
#include <sys/syssgi.h>
#include <errno.h>

#include <dmedia/midi.h>

char *tmplate = "/tmp/.mfpXXXXX";
char *fname;

Player::Player(MainWindow *mainWindow)
{    
    // constructor 
    _state = STOPPED; 
    _mainWindow = mainWindow;  
    fname = strdup(tmplate);
    mktemp(fname);
    arena = usinit(fname);
    unlink(fname);
    semap = usnewsema(arena, 0);
    thefile = 0;
    if (mdInit() <= 0) {
	    fprintf(stderr, "MIDI not initialized\n");
	    exit(0);
    }
    scale_ = 1.0;
    lastpause = 0;
}

Player::~Player()
{
    // destructor    
    unlink(fname);
}


// Commands Issued From UI To Player:

int Player::open(const char *name)
{
    // open named file (assumed to be standard midi file)
	;
	;
	// clean up any file that might already be open
	if (thefile) {
		delete thefile;
		mdClosePort(outport);
	}
	thefile = new MFfile;
	if (thefile->open((char *) name, "rw") < 0) {
		// XXX dialog here for GUI
		fprintf(stderr,"can't open %s: ", name);
		perror("");
		return 0;
	}
	
	thefile->read();
	thefile->rewind();
	setFilenameDisplay(name);
	setSongPosDisplay(0.0);
	setSpeedDisplay(1000);
	_state = STOPPED;

	return 1;
}

void Player::play()
{
	long long nanos;

	if (!thefile) return;

	if (_state == STOPPED) { 
		// open up an outport
		if ((outport = mdOpenOutPort(0)) == NULL) {
			perror("opening midi out port");
			return;
		}
		
		// set it to use non-blocking I/O.  We use non-blocking
		// for output so that stopping a sequence is possible.
		// if we don't, then stopping the sequence will flush what
		// is currently queued, but if mdSend is waiting for room
		// or time to send data that data will be sent as soon as 
		// the currently waitting data is flushed.

		if (fcntl(mdGetFd(outport), F_SETFL, FNONBLOCK) < 0)
			perror("non-blocking");

		// the midi file library stores time stamps relative to
		// the beginning of the file.

		mdSetStampMode(outport, MD_RELATIVETICKS);

	} else if (_state == PAUSED) {
		int tmpo = thefile->gettempo();
		
		mdSetDivision(outport, thefile->division());
		printf("restart: tempo %d division %d\n", thefile->gettempo(), thefile->division());
		mdSetTempo(outport, tmpo);
		nanos = mdTicksToNanos(outport, thefile->tell());

	} else return;

	// set the division & tempo.  division must always be set first,
	// because the constants set in the driver by the tempo change
	// depend on the division.

	mdSetDivision(outport, thefile->division());
	mdSetTempo(outport, thefile->gettempo());

	// set the origin time.  put the the position of the file in a signed 
	// quantity to avoid a compiler warning.
	// the -1 scale on the the time allows the file to start playing in the
	// middle without waiting for the first time stamp to expire.

	nanos = mdTicksToNanos(outport, thefile->tell());
	mdSetOrigin(outport, -1 * nanos);

	_state = PLAYING;

	// set up lastpause accoring to the current position in the file.
	// this quantity is used to set the song position pointer to the
	// right place.

	lastpause = thefile->tell();


	// start up the playback thread & the song position thread.
	// this is a convienance, in many ways.  this could also be 
	// done with a Motif workproc and careful management of the 
	// amount of data written, but its easier to put it into its
	// own thread and let it manage itself.  Same with the song
	// position

	printf("sproc\n");
	if (sproc((void (*)(void *))(Player::pb), PR_SALL, this) < 0) {
		perror("sproc");
		exit(0);
	}

	printf("done\n");

	// the playback thread is waiting on this semaphore.  Releasing
	// it allows the pb thread to start.  When the UI acquires the 
	// semaphore again, it tells playback to stop.
	sginap(100);
	usvsema(semap);

	if ((sppid = sproc((void (*)(void *))(Player::spp), PR_SALL, this)) < 0) {
		fprintf(stderr, "Couldn't start song position thread\n");
		exit(0);
	}

}

// thread to update song position
void
Player::spp(Player *player)
{
	// this thread just loops until the stop button
	// is hit, or the song finishes.  mdTell returns 
	// the number of ticks since the current batch of
	// playback started.  If a sequence is paused, the
	// position resets to 0.  Hence, we have to remember
	// where we started in the sequence, and add it in.

	do {
		player->setSongPosDisplay((1.0 * (player->lastpause + 
						  mdTell(player->outport)) / 
					   player->thefile->length()));
		sginap(10);
	} while (player->_state == PLAYING);
}

#define EVBUFCNT 100
// thread to playback file
void
Player::pb(Player *player)
{
	int nevents;
	MDevent evbuf[EVBUFCNT];	// more than can be needed
	struct pollfd pfd;
	int rpoll;
	int done = 0;
	int nsent = 0;

	// grab the first batch of events...
	nevents = player->thefile->nextevent(evbuf, EVBUFCNT);

	// and wait for the UI process to tell us we're ready to roll
	uspsema(player->semap);

	do {
		// send out the data.
		if ((nsent = mdSend(player->outport, evbuf, nevents)) <= 0) {

			// something bad might have happened...
			if (oserror() != EWOULDBLOCK) {
				player->messageDialog("Failure sending MIDI events");
				perror("mdSend");
				exit(0);
			} else {

				// if we get here, then we have sent more data than
				// we can handle.  

				// first, let the UI know that it is ok to stop us
				usvsema(player->semap);

				// Poll the midi port.  this call will return when 
				// sufficient data has drained to keep us on track

				pfd.fd = mdGetFd(player->outport);
				pfd.events = POLLOUT;
				pfd.revents = 0;

				rpoll = poll(&pfd,1,-1);
				if (rpoll < 0) {
					perror("poll");
					return;
				}

			}
		} else {
		
			// the data went out ok, release the semaphore
			usvsema(player->semap);

			// grab more data
			nevents = player->thefile->nextevent(evbuf, EVBUFCNT);
		}

	// check to see if there was data left in the file, or if the semaphore
	// was acquired by the UI.  uscpsema acquires the semaphore only if it 
	// is available, otherwise, it returns 0.

		done = uscpsema(player->semap);
	} while (nevents && done);

	_exit(0);
}

void Player::stop()
{
	unsigned long long seeker;

	// don't do the work if we're already stopped or there's no
	// file open
	if (_state == PAUSED || _state == STOPPED) return;
	if (!thefile) return;

	// acquire the semaphore, stopping playback
	uspsema(semap);

	// kill off the song position thread, just to be sure.
	kill(sppid, 1);
	_state = PAUSED;

	// pause the port.  this operation returns the timestamp 
	// of the last MIDI message sent out.
	seeker = mdPause(outport);
	mdPanic(outport);

	// add it to the last time we paused, to get the location
	// we want in the file and go there.

	seeker += lastpause;
	thefile->seek(seeker);
}

void Player::rewindBegin()
{
}


void Player::rewindEnd()
{
	if (!thefile) return;

	thefile->rewind();
	_state = STOPPED;

	// close the port, to make sure everything gets 
	// re-initialized.

	mdClosePort(outport);
	setSongPosDisplay(0.0);
}

// fast forward.  start out by remember the
// old tempo scale, and then sent the scale to 
// 4x playback speed.

void Player::forwardBegin()
{
	if (!thefile) return;

	oldspeed = scale_ * 1000;
	setSpeed(4000);
}

// end of fast forward.  reset the tempo scale, 
// and update song position display.

void Player::forwardEnd()
{
	if (!thefile) return;

	setSpeed(oldspeed);
	setSongPosDisplay((1.0 * mdTell(outport)) / 
			  thefile->length());

	setSpeedDisplay(oldspeed);
}

// set the song position.  input it an from 0..1000
// representing 1/10ths of a percent through the file

void Player::setSongPos(int value)
{
	double where = value / 1000.0;
	unsigned long long target;

	if (!thefile) return;

	if (_state == PLAYING) { stop(); }

	target = thefile->length() * where;

	thefile->seek(target);
}

void Player::setSpeed(int value)
{
	if (!thefile) return;

	scale_ = value / 1000.0;
	mdSetTemposcale(outport, scale_);
}

//
// Commands back to UI
//
void Player::setSpeedDisplay(int speed)
{
	if (!thefile) return;

	_mainWindow->setSpeedDisplay(speed);    
}

void Player::setFilenameDisplay(const char *name)
{
	const char *tmp = strrchr(name, '/');
	if (!tmp) tmp = name;
	else tmp++;
	_mainWindow->setFilenameDisplay(tmp);
}

void Player::setSongPosDisplay(double position)
{
	int pos = position * 1000;
	_mainWindow->setSongPosDisplay(pos);
}

void Player::setTextBoxString(const char *string)
{
	_mainWindow->setTextBoxString(string);    
}

void Player::messageDialog(const char *message)
{
	_mainWindow->messageDialog(message);
}


