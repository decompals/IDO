/*
 *   curses1.c:
 *
 *    demostrates switching between block and noblock reading mode via
 *  the keyboard.  The following keys are employed:
 *
 *      F1:  go into blocked read mode
 *      F2:  enter non blocked mode
 *      Q:   quit program (cleans up regardless of current I/O state)
 *
 *  References:  CURSES(3X)
 *
 *                                     Grant Dorman - 1990
 */

#include <curses.h>

main () 
{
    register row, col;
    extern int rand(void);

    initscr(); 
    noecho(); 
    keypad(stdscr, TRUE);
    display(LINES/2-1, COLS/2-4,"Waiting");

    for (;;)
        switch(wgetch(stdscr)) {
            case KEY_F(1): 
		nodelay(stdscr, FALSE); 
		clear();
                display(LINES/2-1, COLS/2-4,"Delay"); 
		break;
            case KEY_F(2): 
	        nodelay(stdscr, TRUE);  
		clear();
                display(LINES/2-1, COLS/2-6, "No Delay"); 
		break;
            case 'q':      
		clear(); 
		refresh(); 
		endwin(); 
		exit(0);
		break;
            case ERR:      
		row = rand() % LINES; 
		col = rand() >> 2 % COLS;
                display(row, col, "" );
		break;
            default:       
		break;
        }
}


display (x, y, string)
int x, y;
char *string;
{ 
    move(x, y); 
    standout(); 
    addstr(string); 
    refresh(); 
}
