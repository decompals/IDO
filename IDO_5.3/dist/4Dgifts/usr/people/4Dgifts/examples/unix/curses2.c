/*
 *   curses2.c:
 *
 *    imitates a "vi"-like text-cursor movement capability in terms of the
 *  'H', 'J', 'K', 'L', as well as the UP/DOWN/LEFT/RIGHT -ARROWKEY
 *  KEYBOARD keys.  pressing the Q key quits the program.
 *
 *  References:  CURSES(3X)
 *
 *                                     Grant Dorman - 1990
 */

#include <curses.h>

main ()

  {
    WINDOW *sub;
    register row, col;

    initscr();
    keypad(stdscr, TRUE);
    crmode();
    nonl();
    noecho();
    clear();
    wrefresh(stdscr);
    for (;;) {
        switch(wgetch(stdscr)) {
            case 'h': 
            case KEY_LEFT: 
                if (col > 0)
	          col--;
                else {
                  if (row > 0) {
                    col = COLS;
                    row -= 1;
	          }
	          else
	            beep();
 	        }
		break;
            case 'j': 
            case KEY_DOWN: 
                if (row < LINES-1)
 	          row++;
                else
                  putchar('');
		break;
            case 'k': 
            case KEY_UP: 
                if (row > 0)
	          row--;
                else
                  putchar('');
		break;
            case 'l': 
            case KEY_RIGHT: 
                if (col < COLS-1)
	          col++;
                else {
                  if (row < LINES-1) {
	            row++;
                    col = 0;
 	          }
	          else
	            putchar('');
	        }
		break;
            case 'q':      
		wclear(stdscr); 
		wrefresh(stdscr); 
		endwin(); 
		exit(0);
            case ERR:      
                mvaddstr(LINES, 1, "BAD slug"); 
		break;
            default:       
		break;
        }
      wmove(stdscr, row, col); 
      wrefresh(stdscr);
    }
}
