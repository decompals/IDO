/*
 * Simple code to support stereo windows
 */

#include <malloc.h>
#include <gl/gl.h>

#include "stereo.h"

typedef struct StereoWindow
{
	long st_id;

	long left_gid, right_gid;

	struct StereoWindow *next, *prev;
} StereoWindow;

static StereoWindow *head = NULL;
static int numwindows = 0;

long
st_winopen(char *title)
{
	long ox, oy, sx, sy;
	StereoWindow *new_window;

	if (numwindows == 0)
	{
		noport();
		winopen(title);	/* Initialize graphics, then... */
		stereo_on();	/* Turn on stereo mode */
	}

	++numwindows;

	new_window = (StereoWindow *)malloc(sizeof(StereoWindow));

	new_window->st_id = numwindows;

	new_window->next = head;
	new_window->prev = NULL;

	head = new_window;

	new_window->right_gid = winopen(title);

	getorigin(&ox, &oy);
	getsize(&sx, &sy);

	/*
	 * Now open up the other window in the right place
	 */
	prefposition(ox, ox+sx, oy+YOFFSET, oy+sy+YOFFSET);
	new_window->left_gid = winopen(title);

	winconstraints();

	return new_window->st_id;
}

static StereoWindow *
find_window(long id)
{
	StereoWindow *scan;
	for (scan = head; scan != NULL; scan = scan->next)
	{
		if (scan->st_id == id) break;
	}
	return scan;
}

void
st_winclose(long id)
{
	StereoWindow *w;
	if ((w = find_window(id)) != NULL)
	{
		/* Unlink from list */
		if (w == head)
			head = w->next;
		if (w->prev != NULL)
			w->prev->next = w->next;
		if (w->next != NULL)
			w->next->prev = w->prev;

		/* Close windows */
		winclose(w->right_gid);
		winclose(w->left_gid);

		/* Free storage */
		free(w);
	}
}

void
st_right(long st_id)
{
	StereoWindow *w;
	if ((w = find_window(st_id)) != NULL)
		winset(w->right_gid);
}

void
st_left(long st_id)
{
	StereoWindow *w;
	if ((w = find_window(st_id)) != NULL)
		winset(w->left_gid);
}

void
st_redraw(short val)
{
	StereoWindow *scan;
	long old_gid;

	old_gid = winget();

	winset(val); reshapeviewport();

	for (scan = head; scan != NULL; scan = scan->next)
	{
		if (scan->right_gid == val) break;
	}
	/*
	 * If it was the right eye window that moved, move the left eye
	 * window to the correct position.
	 *
	 * The left eye will get a redraw later.
	 */
	if (scan != NULL)
	{
		long ox, oy, sx, sy;

		getorigin(&ox, &oy);
		getsize(&sx, &sy);

		winset(scan->left_gid);
		winposition(ox, ox+sx, oy+YOFFSET, oy+sy+YOFFSET);
		reshapeviewport();
	}

	winset(old_gid);
}
