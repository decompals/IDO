/*
// sproc.c:
//
// Demonstrates how to sproc(2) a "share group" process.  The program 
// forks a "share group" process.  The share group process shows that 
// global variables are shared, as well as file descriptors, but 
// automatic varibles are not.  The child process then opens a file and 
// writes a message. After the message is written the child unblocks the 
// parent and goes to sleep.  The parent then examines the contents of 
// the variables and reads the message from the opened file. The parent 
// then sleeps until the child sends a signal.
//
// References: INTRO(2), SIGNAL(2), FORK(2), SPROC(2), PRCTL(2)
//
// George Smith - 1987
// Modifications made by Matt D. Robinson - 1993
//
*/

#include <stdio.h>
#include <fcntl.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/prctl.h>

/*
// User definable variables.  These are the variables which show how
// the program changes.  DATA_INT_1, PARENT_NAME and CHILD_NAME will
// remain consistent in both the child and parent processes.  DATA_INT_2
// and DATA_INT_3, however, will be different, because one is declared
// locally to the parent, and the other is declared globally and is
// accessible by the child.
*/
#define DATA_INT_1  100
#define DATA_INT_2  200
#define DATA_INT_3  200
#define PARENT_NAME "Parent"
#define CHILD_NAME  "Child"

/*
// Declare global variables.  These variables will be open to both
// processes sharing the same data/text space.  Note that here we
// set data_int_2 to DATA_INT_3, which should be different from
// DATA_INT_2.  The reason we do this is to show that when a
// variable is declared automatically on the parent process' stack,
// then the value of that variable is different than the data_int_2
// variable declared globally.  NOTE:  There are *two* declarations
// of data_int_2.  You'll see both of them, one just below, and the
// other in the main() function.
*/
int   data_int_1, data_int_2 = DATA_INT_3, pid;
char  tmp_string[BUFSIZ];
int   fp;

int main(int argc, char **argv)
{
	void die_signal();
	void sproc_group_call();
	int data_int_2;

	/*
	// The first thing to do is to set some basic variables.
	// Our data integer "data_int_1" will be seen by both
	// processes.  Note that we set data_int_2 to DATA_INT_2
	// here, not to DATA_INT_3 as declared globally.
	*/
	data_int_1 = DATA_INT_1;
	data_int_2 = DATA_INT_2;

	/*
	// Define subroutine to be called when SIGUSR1 is recieved.
	*/
	signal(SIGUSR1, die_signal);
	fprintf(stderr, "\nMAKING SPROC CALL...\n\n");
	fflush(stdout);


	/*
	// Fork a share group process.
	*/
	pid = sproc(sproc_group_call, PR_SALL | PR_BLOCK, CHILD_NAME);

	/*
	// At this point, we're in the parent call.  We'll go ahead and
	// sleep for a little bit, and wait for something to happen from
	// the child.  Make sure to inform the user that the parent
	// process is going to sleep briefly.
	*/
	printf("(%s):\n\tDATA: [data_int_1 = %d]\n\tDATA: [data_int_2 = %d]\n"
		"\tSLEEPING WAITING FOR SIGNAL FROM (%s) TO READ DATA.\n\n",
		PARENT_NAME, data_int_1, data_int_2, CHILD_NAME);
	fflush(stdout);
	
	/*
	// At this point, we shall read from the file that was created
	// by the child process.  We shall also print out what was in
	// that message.
	*/
	lseek(fp, 0, 0);
	read(fp, tmp_string, 35);
	close(fp);
	unlink("sproc_data_file");
	printf("(%s):\n\tRECEIVED MESSAGE FROM (%s): [%s]\n\n", PARENT_NAME,
		CHILD_NAME, tmp_string);
	fflush(stdout);

	/*
	// Go to sleep for a little bit, then return 0.
	*/
	sleep(30);
	exit(0);
}


void sproc_group_call(char *name)
{
	int ppid;

	/*
	// Let's find out first what our parent process ID is.
	*/
	ppid = getppid();

	/*
	// Print out our initial data information.  This should show the
	// user the differences between our variables in the parent and
	// in the child.
	*/
	printf("(%s):\n\tDATA: [data_int_1 = %d]\n\tDATA: [data_int_2 = %d]\n\n",
		name, data_int_1, data_int_2);
	fflush(stdout);

	/*
	// Now we open up a bogus file, put some information into it, and
	// close it.
	*/

	fp = open("sproc_data_file", O_RDWR | O_CREAT, 0666);
	write(fp, "I was opened by sproc_group_call().", 35);

	/*
	// Print out that we sent the message.
	*/
	printf("(%s):\n\tSENDING MESSAGE TO (%s): [%s]\n\n", name,
		PARENT_NAME, "I was opened by sproc_group_call().");
	fflush(stdout);

	/*
	// Unblock the parent process.
	*/
	unblockproc(ppid);

	/*
	// Change our data_int_1 variable to show that the value changes.
	*/
	data_int_1++;
	sleep(2);

	printf("(%s):\n\tSENDING SIGNAL TO KILL OFF (%s)\n\tSIGNAL: [%d]\n\t"
		"DATA: [data_int_1 should be equal to %d in (%s)]\n\n",
		name, PARENT_NAME, SIGUSR1, data_int_1, PARENT_NAME);
	fflush(stdout);

	/*
	// Send the kill signal to the parent process, then exit.
	*/
	kill(ppid, SIGUSR1);
	exit(0);        
}


void die_signal(sig) 
int sig;
{
	/*
	// Reset our signal.
	*/
	signal(SIGUSR1, die_signal);
	printf("(%s)\n\tRECEIVED KILL SIGNAL FROM (%s)\n\tSIGNAL: [%d]\n"
		"\tDATA: [data_int_1 = %d]\n\n", PARENT_NAME, CHILD_NAME,
		sig, data_int_1);
	return;
}
