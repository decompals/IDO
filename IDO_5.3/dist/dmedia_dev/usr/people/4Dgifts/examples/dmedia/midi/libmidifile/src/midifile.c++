#include <dmedia/midi.h>
#include <midifile.h>
#include <miditrack.h>
#include <libc.h>
#include <values.h>
#include <sys/time.h>
#include <signal.h>
#include <stdio.h>

MFfile::MFfile()
{
    lasttime_ = MAXINT;
    ntracks_ = 0;		// default no tracks
    tempo_ = 0;
    division_ = 120;
    tracks_ = 0;
    inited_ = 0;
    length_ = 0;
}

void
MFfile::doinit()
{
    if (inited_) return;

    return;
}

MFfile::~MFfile()
{
}

// return a pointer to the requested track in the midifile.
// if the track doesn't exist (ie, has been deleted) make a new one
MFtrack *
MFfile::gettrack(int n)
{
    if (n < ntracks_) {
	if (!tracks_[n])
	    tracks_[n] = new MFtrack(this);
	return tracks_[n];
    } else
	return 0;
}

void
MFfile::addtrack(MFtrack *track)
{
	for (int i = 0; i < ntracks_; i++) {
		if (!tracks_[i]) {
			tracks_[i] = track;
			return;
		}
	}

	tracks_ = 
	    (MFtrack **) realloc(tracks_, (ntracks_ + 1) * sizeof(MFtrack *));
	
	tracks_[i] = track;
	ntracks_++;
}

void
MFfile::deletetrack(int n)
{
    if (n < ntracks_) {
	delete tracks_[n];
	tracks_[n] = 0;
    }
}

static int
metatotempo(char *msg)
{
	return (msg[3] << 16) + (msg[4] << 8) + msg[5];
}

// return all tracks which have an event on the
// next active tick.
int 
MFfile::nextevent(MDevent *evlist, int /*nevent*/)
{
	unsigned long long low = 0xffffffffffffffff;
	int nev = 0;
	int i;

	// find the lowest numbered tick on all the tracks
	for (i = 0; i < ntracks_; i++) {
		unsigned long long tmptime = tracks_[i]->firststamp();
		if (tmptime < low)
			low = tmptime;
	}
	
	for (i = 0; i < ntracks_; i++) {
		unsigned long long tmptime = tracks_[i]->firststamp();
		if (tmptime == low) {
			int hm = tracks_[i]->Howmany();
			for (int j = 0; j < hm; j++) {
				evlist[nev] = *(tracks_[i]->nextevent());
				nev++;
			}
		}
	}
	
	lasttime_ = low;
	return nev;
}

// get the current tempo for a file.  looks in the tempo
// map for type 1 files.  for now, assumes that type 0 files
// have no tempo changes.


int 
MFfile::gettempo(void)
{
	return tempo_;
}

unsigned long long
MFfile::tell()
{
	unsigned long long tmp  = 0xffffffffffffffff;
	for (int i = 1; i < ntracks_; i++) 
		if (tracks_[i]->firststamp() < tmp)
			tmp = tracks_[i]->firststamp();

	return tmp;
}

void
MFfile::rewind()
{
    lasttime_ = 0;
    for (int i = 0; i < ntracks_; i++)
	if (tracks_[i])
	    tracks_[i]->rewind();
}

void
MFfile::tempo(int t)
{
    tempo_ = t;
}

MFfile::open(char *name, char *mode)
{
    fp_ = fopen(name, mode);
    if (fp_ == NULL) return -1;
    return 0;
}

MFfile::create(char *name)
{
	fp_ = fopen(name, "w");
	if (fp_ == NULL) return -1;
	return 0;
}

// XXX all "read" methods need corresponding "write" methods.

// readhdr() -- reads the header, returning the number of tracks.
// sets any appropriate private variables, such as timing, etc...
//
MFfile::read() 
{
	int tot = 0;
    ntracks_ = readfilehdr();
    
    tracks_ = (MFtrack **) calloc(ntracks_, sizeof(MFtrack *));

    for (int i = 0; i < ntracks_; i++) {
	tot += readtrack(&tracks_[i]);
}
    return 0;
}

MFfile::readhd(char *looking, int skip)
{
    int c;

    if (skip)
	do {
	    c = readchar();
	} while (c != EOF && c != looking[0]);
    else 
	c = readchar();

    if (c != looking[0]) return 0;
    c = readchar();
    if (c != looking[1]) return 0;
    c = readchar();
    if (c != looking[2]) return 0;
    c = readchar();
    if (c != looking[3]) return 0;
    return 1;
}

// read a file header.

MFfile::readfilehdr()
{
    if (!readhd("MThd",0))
	return 0;
    toread_ = read32();
    format_ = read16();
    ntracks_ = read16();
    division_ = read16();  // XXX ticks per quarter??  get more info...
    if (division_ < 0) {
	    printf("file in SMPTE format\n");
    }
    while (toread_ > 0) readchar();
    return ntracks_;
}

// read a track header

MFfile::readtrackhdr()
{
    if (!readhd("MTrk",1))
	return 0;
    toread_ = read32();
    return toread_;
}

// read a single character

MFfile::readchar() 
{
    int c = getc(fp_);
    toread_--;
    return c;
}

// read a 32 bit number

MFfile::read32()
{
    int value, c;
    c = readchar();
    value = c & 0xff;
    c = readchar();
    value = (value << 8) + (c & 0xff);
    c = readchar();
    value = (value << 8) + (c & 0xff);
    c = readchar();
    value = (value << 8) + (c & 0xff);
    return value;
}

// read a 16 bit number 

MFfile::read16()
{
    int value, c;
    c = readchar();
    value = c & 0xff;
    c = readchar();
    value = (value << 8) + (c & 0xff);
    return value;
}

// read a variable length number, not over 28 bits

MFfile::readvar()
{
    int value, c;
    
    c = readchar();
    value = c;
    if (c & 0x80) {
	value &= 0x7f;
	do {
	    c = readchar();
	    value = (value << 7) + (c & 0x7f);
	} while (c & 0x80);
    }
    return value;
}

void
MFfile::write32(int n)
{
    int mask = 0xff000000;
    fprintf(fp_,"%c",(n & mask) >> 24); mask >>= 8;
    fprintf(fp_,"%c",(n & mask) >> 16); mask >>= 8;
    fprintf(fp_,"%c",(n & mask) >> 8); mask >>= 8;
    fprintf(fp_,"%c",(n & mask));
}

void
MFfile::write16(int n)
{
    int mask = 0xff00;
    fprintf(fp_,"%c",(n & mask) >> 8); mask >>= 8;
    fprintf(fp_,"%c",(n & mask));
}

int
MFfile::writevar(int n)
{
    char b[4];
    int mask = 0x0f700000;
    int i = -1;
    int sum = 0;

    b[0] = 0x80 | ((n & mask) >> 21); mask = 0x1fc000; 
    if (b[0] & 0x7f) i = 0;
    b[1] = 0x80 | ((n & mask) >> 14); mask = 0x3f80; 
    if ((i == -1) && (b[1] & 0x7f)) i = 1;
    b[2] = 0x80 | ((n & mask) >> 7 ); mask = 0x7f; 
    if ((i == -1) && (b[2] & 0x7f)) i = 2;
    b[3] = (n & mask) & 0x7f; if ((i == -1)) i = 3;
    
    for ( ; i < 4; i++) {
	fprintf(fp_,"%c",b[i]);
	sum++;
    }
    return sum;
}

int
MFfile::writetime(int t)
{
    return writevar(t);
}

int
MFfile::newtrack()
{
    if (tracks_)
	tracks_ = 
	    (MFtrack **) realloc(tracks_, (ntracks_ + 1) * sizeof(MFtrack *));
    else {
	tracks_ = (MFtrack **) calloc(1, sizeof(MFtrack *));
    }

    tracks_[ntracks_] = new MFtrack(this);
    ntracks_++;
    return ntracks_ - 1;
}

MFfile::writeevent(MDevent *ev)
{
    int sum = 0, length;
    char *m;
    sum = writetime(ev->stamp);
    length = ev->msglen;
    if (length > 3) 
	    m = ev->sysexmsg;
    else
	    m = ev->msg;

    for (int i = 0; i < length; i++) 
	    fprintf(fp_,"%c", m[i]);
    
    return sum + length;
}

// munge the time signature...
void
MFfile::timesig(char a, char b, char c, char d)
{
    denom_ = 1;
    while (b-- > 0)
	denom_ *= 2;
    numer_ = a;
    ppq_ = c;
    thirt2s_ = d;
}

// for type 1 files, set the music tracks to
// the time, the chase the tempo track to get
// it to the right place.
void 
MFfile::seek(unsigned long long time) 
{
	for (int i = 1; i < ntracks_; i++)
		tracks_[i]->seektime(time);

	tracks_[0]->seektime(0);

	while (time > tracks_[0]->firststamp()) {
		       
		tempo_ = metatotempo(tracks_[0]->currentevent()->sysexmsg);
		tracks_[0]->nextevent();
	}

	lasttime_ = time;
}
// read an entire track from the file, stuffing results into an MFtrack

MFfile::readtrack(MFtrack **track)
{
    int running = 0;
    int runn = 0;
    int needed, i;
    char mess[3];
    int status;
    int time, lasttime_ = 0;
    MDevent foo, *tmp = &foo;
    int evcount = 0;

    if (!*track) 
	*track = new MFtrack(this);

    MFtrack *t = *track;
    
    readtrackhdr();
    time = 0;

    while (toread_ > 0) {
	time = readvar();
	status = readchar() & 0xff;
	lasttime_ += time;

	if (status < MD_SYSEX) {
		if (!(status & 0x80)) {	// running status 
			mess[1] = status;
			status = running;
			runn = 1;
		} else {
			running = status;
			runn = 0;
		}

		needed = mdMsgLen(status);
		mess[0] = status;
		
		// fill in the rest of the message.
		// if running status in effect, 
		//   read mess[2],
		//   otherwise read mess[1] and mess[2] if needed
		// if the message is only 2 bytes, the condition
		// fails, and no more data is read
		
		int i = 1 + runn;
		while (i < needed) {
			mess[i++] = readchar() & 0xff;
		}

		tmp->stamp = lasttime_;
		tmp->msglen = mdMsgLen(mess[0]);
		bcopy(mess, tmp->msg, tmp->msglen);
		t->insertevent(tmp);
		evcount++;
	} else {		// not a channel message
	    switch(status) {
	    default:
		    printf("non-channel message\n");
	    case MD_SYSEX:	// for now, ignore sysex in files
	    case MD_EOX:
	    {
		int readmore = readvar();
		for (i = 0; i < readmore; i++) 
			readchar();
		break;
	    }
	    case MD_META: {
		int type = readchar();
		int readmore = readvar();

		switch(type) {
		case MIDImeta_SeqNumber:
		    printf("sequence # %d\n", read16());
		    break;
		case MIDImeta_Text:
		case MIDImeta_Copyright:
		case MIDImeta_Name:
		case MIDImeta_Instrument:
		case MIDImeta_Lyric:
		case MIDImeta_Marker:
		case MIDImeta_CuePoint:
		    for (i = 0; i < readmore; i++)
			printf("%c",readchar());
		    printf("\n");
		    break;
		case MIDImeta_ChannelPrefix:
		    break;
		case MIDImeta_EOT:
		    if (lasttime_ > length_)
			    length_ = lasttime_;
		    break;
		case MIDImeta_SetTempo:	
		{
		    char *msg = (char *) malloc(6); 
		    msg[0] = MD_META;
		    msg[1] = MIDImeta_SetTempo;
		    msg[2] = 03;
		    msg[3] = readchar();
		    msg[4] = readchar();
		    msg[5] = readchar();
		    if (!tempo_)
			    tempo_ = metatotempo(msg);
		    tmp->stamp = lasttime_;
		    tmp->sysexmsg = msg;
		    tmp->msglen = 6;
		    t->insertevent(tmp);
		    evcount++;
		}
		    
		    break;
		case MIDImeta_SMPTEoffset:
		    for (i = 0; i < readmore; i++)
			printf(":%d", readchar());
		    printf("\n");
		    break;
		case MIDImeta_TimeSignature:
		    timesig(readchar(), readchar(), readchar(), readchar());
		    printf("time sig change\n");
		    break;
		case MIDImeta_KeySignature:
		    printf("sharps/flats: %d, minor: %d\n",
			   readchar(), readchar());
		    break;
                default:
		    for (i = 0; i < readmore; i++)
			    readchar();
		}

		break;
	        }
	    }
	}
    }
    return evcount;
}

int 
MFfile::writemeta(MIDImetaEvents meta, ...)
{
    int sum = 0;
    fprintf(fp_,"%c%c%c", MD_META, 1, meta);
    sum = 2;
    switch(meta) {
    case MIDImeta_EOT:
	break;
    case MIDImeta_SetTempo:
	break;
    }
    return sum;
}
		  
int
MFfile::writehd(char *looking)
{
    return fprintf(fp_,"%s",looking);
}

void
MFfile::writefilehdr()
{
    writehd("MThd");
    write32(6);
    write16(ntracks_ > 1 ? 1 : 0);
    write16(ntracks_);
    write16(division_);
}

void
MFfile::writetrackhdr()
{
    writehd("MTrk");
    backtrack_ = ftell(fp_);
    write32(0);
}

int
MFfile::writetrack(int trck)
{
    MDevent *ev;
    MFtrack *trak = tracks_[trck];
    int sum = 0, nev = 0;
    MDevent evn;
    char eot[] = {0xff, 0x2f, 0x00};

    writetrackhdr();
    trak->seektime(0);


    ev = trak->nextevent();
    do {
	if (ev) {
	    sum += writeevent(ev);
	    nev++;
        }
	else
	    break;
    } while (ev = trak->nextevent());

    printf("wrote %d\n", nev);
    bcopy(eot,evn.msg, 3);
    evn.stamp = 0;
    evn.msglen = 3;

    sum += writeevent(&evn);
    long here = ftell(fp_);
    fseek(fp_,backtrack_,0);
    write32(sum);
    fseek(fp_,here,0);
    return sum;
}

void
MFfile::writefile()
{
    ::rewind(fp_);
    writefilehdr();
    printf("ntracks %d\n", ntracks_);
    for (int i = 0; i < ntracks_; i++) {
	    printf("writing track %d\n", i);
	if (tracks_[i])
	    writetrack(i);
}
    fclose(fp_);
}

unsigned long long
MFfile::length() 
{
	return length_;
}

