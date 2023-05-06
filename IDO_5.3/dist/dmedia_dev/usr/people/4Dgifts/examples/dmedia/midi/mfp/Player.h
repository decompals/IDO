/*
 * Player.h
 */
 
#ifndef __PLAYER_H__
#define __PLAYER_H__

#include <iostream.h>

#include "defs.h"

#include <dmedia/midi.h>
#include "midifile.h"

#include <ulocks.h>

class MainWindow;

#define IDLE      0
#define RECORDING 1
#define PLAYING   2
#define PAUSED    3
#define STOPPED   4

class Player {

    public:
	Player(MainWindow *mainWindow);
	~Player();
	
	// Commands _from_ User Interface

	int	    open(const char* pathname);
	void	    play();
	void	    stop();
	void	    rewindBegin();
	void	    rewindEnd();
	void	    forwardBegin();
	void	    forwardEnd();
	void	    setSpeed(int value);
	void	    setSongPos(int value);
	void	    speedPos(int value);


	// Commands _to_ User Interface
			
	void	    setFilenameDisplay(const char *name);
	void	    setSpeedDisplay(int speed);
	void	    setSongPosDisplay(double position);
	void	    setTextBoxString(const char *string);
	void	    messageDialog(const char *message);


	// playback process
	static void pb(Player *player);

	// song position updater
	static void spp(Player *player);

    private:
	MainWindow  *_mainWindow;   // for communication back to UI
	volatile char _state;    
	volatile MDport outport;
	MFfile *thefile;
	usptr_t *arena;
	usema_t *semap;
	double scale_;
	int oldspeed;
	unsigned long long target;
	unsigned long long lastpause;
	int sppid;
};

#endif
// __PLAYER_H__
