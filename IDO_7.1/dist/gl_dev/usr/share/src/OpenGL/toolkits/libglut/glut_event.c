/* Copyright (c) Mark J. Kilgard, 1994, 1995, 1996. */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#ifdef __sgi
#include <bstring.h>    /* prototype for bzero used by FD_ZERO */
#endif
#ifdef AIXV3
#include <sys/select.h> /* select system call interface */
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#ifdef __hpux
/* XXX Bert Gijsbers <bert@mc.bio.uva.nl> reports that HP-UX
   needs different keysyms for the End, Insert, and Delete keys
   to work on an HP 715.  It would be better if HP generated
   standard keysyms for standard keys. */
#include <X11/HPkeysym.h>
#endif
#include <GL/glut.h>
#include "glutint.h"

static GLUTtimer *freeTimerList = NULL;
static int mappedMenuButton;
static Window menuWindow;

GLUTidleCB __glutIdleFunc = NULL;
GLUTtimer *__glutTimerList = NULL;
#ifdef SUPPORT_FORTRAN
GLUTtimer *__glutNewTimer;
#endif
GLUTwindow *__glutWindowWorkList = NULL;
void (*__glutUpdateInputDeviceMaskFunc) (GLUTwindow *);

void
glutIdleFunc(GLUTidleCB idleFunc)
{
  __glutIdleFunc = idleFunc;
}

void
glutTimerFunc(unsigned long interval, GLUTtimerCB timerFunc, int value)
{
  GLUTtimer *timer, *other;
  GLUTtimer **prevptr;
  struct timeval now;

  if (!timerFunc)
    return;

  if (freeTimerList) {
    timer = freeTimerList;
    freeTimerList = timer->next;
  } else {
    timer = (GLUTtimer *) malloc(sizeof(GLUTtimer));
    if (!timer)
      __glutFatalError("out of memory.");
  }

  timer->func = timerFunc;
  timer->timeout.tv_sec = interval / 1000;
  timer->timeout.tv_usec = (interval % 1000) * 1000;
  timer->value = value;
  timer->next = NULL;
  GETTIMEOFDAY(&now);
  ADD_TIME(timer->timeout, timer->timeout, now);
  prevptr = &__glutTimerList;
  other = *prevptr;
  while (other && IS_AFTER(other->timeout, timer->timeout)) {
    prevptr = &other->next;
    other = *prevptr;
  }
  timer->next = other;
#ifdef SUPPORT_FORTRAN
  __glutNewTimer = timer;  /* for Fortran binding! */
#endif
  *prevptr = timer;
}

static void
handleTimeouts(void)
{
  struct timeval now;
  GLUTtimer *timer;

  if (__glutTimerList) {
    GETTIMEOFDAY(&now);
    while (IS_AT_OR_AFTER(__glutTimerList->timeout, now)) {
      timer = __glutTimerList;
      (*timer->func) (timer->value);
      __glutTimerList = timer->next;
      timer->next = freeTimerList;
      freeTimerList = timer;
      if (!__glutTimerList)
        break;
    }
  }
}

void
__glutPutOnWorkList(GLUTwindow * window, int workMask)
{
  if (window->workMask) {
    /* Already on list; just OR in new workMask. */
    window->workMask |= workMask;
  } else {
    /* Update work mask and add to window work list. */
    window->workMask = workMask;
    window->prevWorkWin = __glutWindowWorkList;
    __glutWindowWorkList = window;
  }
}

static void
postRedisplay(GLUTwindow * window)
{
  /**
   * Only really post a redisplay if:
   *     1) There is a display func registered;
   *     2) The window is known to be currently mapped;
   * and 3) The visibility is visible (or the visibility of
   *         the window is unknown,
   *         ie. window->visState == -1).
   */
  if (window->display && window->mapState &&
    window->visState != 0) {
    __glutPutOnWorkList(window, GLUT_REDISPLAY_WORK);
  }
}

/* CENTRY */
void
glutPostRedisplay(void)
{
  postRedisplay(__glutCurrentWindow);
}

/* ENDCENTRY */

#if (GLUT_API_VERSION >= 2)
static EventParser *eventParserList = NULL;

/* __glutRegisterEventParser allows another module to register
   to intercept X events types not otherwise acted on by the
   GLUT processEvents routine.  The X Input extension support
   code uses an event parser for handling X Input extension
   events.  */

void
__glutRegisterEventParser(EventParser * parser)
{
  parser->next = eventParserList;
  eventParserList = parser;
}

#endif /* (GLUT_API_VERSION >= 2) */
static void
processEvents(void)
{
  XEvent event, ahead;
  GLUTwindow *window;
  int width, height;
  EventParser *parser;

  do {
    XNextEvent(__glutDisplay, &event);
    switch (event.type) {
    case MappingNotify:
      XRefreshKeyboardMapping((XMappingEvent *) & event);
      break;
    case ConfigureNotify:
      window = __glutGetWindow(event.xconfigure.window);
      if (window) {
        width = event.xconfigure.width;
        height = event.xconfigure.height;
        if (width != window->width || height != window->height) {
          window->width = width;
          window->height = height;
          __glutSetWindow(window);
          (*window->reshape) (width, height);
          window->forceReshape = False;
        }
      }
      break;
    case Expose:
      /* compress expose events */
      while (XEventsQueued(__glutDisplay, QueuedAfterReading)
        > 0) {
        XPeekEvent(__glutDisplay, &ahead);
        if (ahead.type != Expose ||
          ahead.xexpose.window != event.xexpose.window)
          break;
        XNextEvent(__glutDisplay, &event);
      }
      if (event.xexpose.count == 0) {
        GLUTmenu *menu;

        if (__glutMappedMenu &&
          (menu = __glutGetMenu(event.xexpose.window))) {
          __glutPaintMenu(menu);
        } else {
          window = __glutGetWindow(event.xexpose.window);
          if (window) {
            postRedisplay(window);
          }
        }
      } else {
        /* there are more exposes to read; wait to redisplay */
      }
      break;
    case ButtonPress:
    case ButtonRelease:
      if (__glutMappedMenu && event.type == ButtonRelease
        && mappedMenuButton == event.xbutton.button) {
        /* Menu is currently popped up and its button is
           released. */
        __glutFinishMenu(event.xbutton.window, event.xbutton.x, event.xbutton.y);
      } else {
        window = __glutGetWindow(event.xbutton.window);
        if (window) {
          GLUTmenu *menu;

          menu = __glutGetMenuByNum(
            window->menu[event.xbutton.button - 1]);
          if (menu) {
            if (event.type == ButtonPress && !__glutMappedMenu) {
              menuWindow = event.xbutton.window;
              __glutStartMenu(menu, window,
                event.xbutton.x_root, event.xbutton.y_root,
                event.xbutton.x, event.xbutton.y);
              mappedMenuButton = event.xbutton.button;
            } else {
              /* Ignore a release of a button with a menu
                 attatched to it when no menu is popped up, or
                 ignore a press when another menu is already
                 popped up. */
            }
          } else if (window->mouse) {
            __glutSetWindow(window);
            (*window->mouse) (event.xbutton.button - 1,
              event.type == ButtonRelease ?
              GLUT_UP : GLUT_DOWN,
              event.xbutton.x, event.xbutton.y);
          } else {
            /* Stray mouse events.  Ignore. */
          }
        } else {
          /* Window might have been destroyed and all the 
             events for the window may not yet be received. */
        }
      }
      break;
    case MotionNotify:
      if (!__glutMappedMenu) {
        window = __glutGetWindow(event.xmotion.window);
        if (window) {
          /* If motion function registered _and_ buttons held *
             down, call motion function...  */
          if (window->motion && event.xmotion.state &
            (Button1Mask | Button2Mask | Button3Mask)) {
            __glutSetWindow(window);
            (*window->motion) (event.xmotion.x, event.xmotion.y);
          }
          /* If passive motion function registered _and_
             buttons not held down, call passive motion
             function...  */
          else if (window->passive &&
              ((event.xmotion.state &
                  (Button1Mask | Button2Mask | Button3Mask)) ==
              0)) {
            __glutSetWindow(window);
            (*window->passive) (event.xmotion.x,
              event.xmotion.y);
          }
        }
      } else {
        /* Motion events are thrown away when a pop up menu is
           active. */
      }
      break;
    case KeyPress:
      window = __glutGetWindow(event.xkey.window);
      if (window && window->keyboard) {
        char tmp[1];
        int rc;

        rc = XLookupString(&event.xkey, tmp, sizeof(tmp),
          NULL, NULL);
        if (rc) {
          __glutSetWindow(window);
          (*window->keyboard) (tmp[0],
            event.xkey.x, event.xkey.y);
        }
#if (GLUT_API_VERSION >= 2)
        else if (window->special) {
          KeySym ks;
          int key;

          ks = XLookupKeysym((XKeyEvent *) & event, 0);
          /* XXX Verbose, but makes no assumptions about keysym 

             layout. */
          switch (ks) {
            /* function keys */
          case XK_F1:
            key = GLUT_KEY_F1;
            break;
          case XK_F2:
            key = GLUT_KEY_F2;
            break;
          case XK_F3:
            key = GLUT_KEY_F3;
            break;
          case XK_F4:
            key = GLUT_KEY_F4;
            break;
          case XK_F5:
            key = GLUT_KEY_F5;
            break;
          case XK_F6:
            key = GLUT_KEY_F6;
            break;
          case XK_F7:
            key = GLUT_KEY_F7;
            break;
          case XK_F8:
            key = GLUT_KEY_F8;
            break;
          case XK_F9:
            key = GLUT_KEY_F9;
            break;
          case XK_F10:
            key = GLUT_KEY_F10;
            break;
          case XK_F11:
            key = GLUT_KEY_F11;
            break;
          case XK_F12:
            key = GLUT_KEY_F12;
            break;
            /* directional keys */
          case XK_Left:
            key = GLUT_KEY_LEFT;
            break;
          case XK_Up:
            key = GLUT_KEY_UP;
            break;
          case XK_Right:
            key = GLUT_KEY_RIGHT;
            break;
          case XK_Down:
            key = GLUT_KEY_DOWN;
            break;
          case XK_Prior:  /* same as X11R6's XK_Page_Up */
            key = GLUT_KEY_PAGE_UP;
            break;
          case XK_Next:  /* same as X11R6's XK_Page_Down */
            key = GLUT_KEY_PAGE_DOWN;
            break;
          case XK_Home:
            key = GLUT_KEY_HOME;
            break;
          case XK_End:
#ifdef __hpux
          case XK_Select:
#endif
            key = GLUT_KEY_END;
            break;
          case XK_Insert:
#ifdef __hpux
          case XK_InsertChar:
#endif
            key = GLUT_KEY_INSERT;
            break;
#ifdef __hpux
          case XK_DeleteChar:
            /* The Delete character is really an ASCII key. */
            tmp[0] = 127;
            __glutSetWindow(window);
            (*window->keyboard) (tmp[0],
              event.xkey.x, event.xkey.y);
            goto skip;
#endif
          default:
            goto skip;
          }
          __glutSetWindow(window);
          (*window->special) (key, event.xkey.x, event.xkey.y);
        skip:;
        }
#endif
      }
      break;
    case EnterNotify:
    case LeaveNotify:
      if (event.xcrossing.mode == NotifyNormal) {
        if (__glutMappedMenu) {
          GLUTmenuItem *item;
          int num;

          item = __glutGetMenuItem(__glutMappedMenu,
            event.xcrossing.window, &num);
          if (item) {
            __glutMenuItemEnterOrLeave(item, num, event.type);
            break;
          }
        }
        window = __glutGetWindow(event.xcrossing.window);
        if (window) {
          if (window->entry) {
            __glutSetWindow(window);
            if (event.type == EnterNotify) {
              int num = window->num;
              Window xid = window->win;

              (*window->entry) (GLUT_ENTERED);

              if (__glutMappedMenu) {

                /* Do not generate any passive motion events
                   when menus are in use. */

              } else {

                /* An EnterNotify event can result in a
                   "compound" callback if a passive motion
                   callback is also registered. In this case,
                   be a little paranoid about the possibility
                   the window could have been destroyed in the
                   entry callback. */

                window = __glutWindowList[num];
                if (window && window->passive && window->win == xid) {
                  __glutSetWindow(window);
                  (*window->passive) (event.xcrossing.x, event.xcrossing.y);
                }
              }
            } else {
              (*window->entry) (GLUT_LEFT);
            }
          } else if (window->passive) {
            __glutSetWindow(window);
            (*window->passive) (event.xcrossing.x, event.xcrossing.y);
          }
        }
      } else {
        /* Careful to ignore Enter/LeaveNotify events that come
           from the pop-up window pointer grab and ungrab. */
      }
      break;
    case MapNotify:
    case UnmapNotify:
      /* "event.xmap.window" is safely assumed to be the same
         element as "event.xunmap.window" */
      window = __glutGetWindow(event.xmap.window);
      if (window) {
        window->mapState = (event.type != UnmapNotify);
        if (window->visibility) {
          if (window->mapState != window->visState) {
            window->visState = window->mapState;
            __glutSetWindow(window);
            (*window->visibility) (window->visState ?
              GLUT_VISIBLE : GLUT_NOT_VISIBLE);
          }
        }
      }
      break;
    case VisibilityNotify:
      window = __glutGetWindow(event.xvisibility.window);
      if (window && window->visibility) {
        int visState =
        (event.xvisibility.state != VisibilityFullyObscured);

        if (visState != window->visState) {
          window->visState = visState;
          __glutSetWindow(window);
          (*window->visibility) (visState ?
            GLUT_VISIBLE : GLUT_NOT_VISIBLE);
        }
      }
      break;
    case ClientMessage:
      if (event.xclient.data.l[0] == __glutWMDeleteWindow)
        exit(0);
      break;
#if (GLUT_API_VERSION >= 2)
    default:
      /* Pass events not directly handled by the GLUT main
         event loop to any event parsers that have been
         registered.  In this way, X Input extension events are
         passed to the correct handler without forcing all GLUT
         programs to support X Input event handling. */
      parser = eventParserList;
      while (parser) {
        if ((*parser->func) (&event))
          break;
        parser = parser->next;
      }
      break;
#endif
    }
  }
  while (XPending(__glutDisplay));
}

static void
waitForSomething(void)
{
  static struct timeval zerotime =
  {0, 0};
  struct timeval now, timeout, waittime;
  fd_set fds;
  int rc;

  /* flush X protocol since XPending does not do this
     implicitly */
  XFlush(__glutDisplay);
  if (XPending(__glutDisplay)) {
    /* It is possible (but quite rare) that XFlush may have
       needed to wait for a writable X connection file
       descriptor, and in the process, may have had to read off
       X protocol from the file descriptor. If XPending is true,
       this case occured and we should avoid waiting in select
       since X protocol buffered within Xlib is due to be
       processed and potentially no more X protocol is on the
       file descriptor, so we would risk waiting improperly in
       select. */
    goto immediatelyHandleXinput;
  }
  FD_ZERO(&fds);
  FD_SET(__glutConnectionFD, &fds);
  timeout = __glutTimerList->timeout;
  GETTIMEOFDAY(&now);
  if (IS_AFTER(now, timeout)) {
    TIMEDELTA(waittime, timeout, now);
  } else {
    waittime = zerotime;
  }
  rc = select(__glutConnectionFD + 1, &fds,
    NULL, NULL, &waittime);
  if (rc < 0 && errno != EINTR)
    __glutFatalError("select error.");
  /* Without considering the cause of select unblocking, check
     for pending X events *and* then handle any timeouts. We
     always look for X events even if select returned with 0
     (indicating a timeout); otherwise we risk starving X event
     processing by continous timeouts. */
  while (XPending(__glutDisplay)) {
  immediatelyHandleXinput:
    processEvents();
  }
  handleTimeouts();
}

static void
idleWait(void)
{
  while (XPending(__glutDisplay)) {
    processEvents();
  }
  if (__glutTimerList)
    handleTimeouts();
  /* Make sure idle func still exists! */
  if (__glutIdleFunc)
    (*__glutIdleFunc) ();
}

static GLUTwindow *
processWindowWorkList(GLUTwindow * window)
{
  int workMask;

  if (window == NULL) {
    return NULL;
  }
  window->prevWorkWin = processWindowWorkList(window->prevWorkWin);

  /* Capture work mask for work that needs to be done to this
     window, then clear the window's work mask (excepting the
     dummy work bit, see below).  Then, process the captured
     work mask.  This allows callbacks in the processing the
     captured work mask to set the window's work mask for
     subsequent processing. */

  workMask = window->workMask;
  assert(workMask | GLUT_DUMMY_WORK == 0);

  /* Set the dummy work bit, clearing all other bits, to
     indicate that the window is currently on the window work
     list _and_ that the window's work mask is currently being
     processed.  This convinces __glutPutOnWorkList that this
     window is on the work list still. */
  window->workMask = GLUT_DUMMY_WORK;

  /* Be sure to set event mask *BEFORE* map window is done. */
  if (workMask & GLUT_EVENT_MASK_WORK) {
    XSetWindowAttributes attrs;

    attrs.event_mask = window->eventMask;
    XChangeWindowAttributes(__glutDisplay, window->win,
      CWEventMask, &attrs);
  }
#if (GLUT_API_VERSION >= 2)
  /* Be sure to set device mask *BEFORE* map window is done. */
  if (workMask & GLUT_DEVICE_MASK_WORK) {
    (*__glutUpdateInputDeviceMaskFunc) (window);
  }
#endif
  /* Be sure to configure window *BEFORE* map window is done. */
  if (workMask & GLUT_CONFIGURE_WORK) {
    XWindowChanges changes;

    changes.x = window->desiredX;
    changes.y = window->desiredY;
    changes.width = window->desiredWidth;
    changes.height = window->desiredHeight;
    changes.stack_mode = window->desiredStack;
    XConfigureWindow(__glutDisplay, window->win,
      window->desiredConfMask, &changes);
    window->desiredConfMask = 0;
  }
  /* Be sure to establish the colormaps *BEFORE* map window is
     done. */
  if (workMask & GLUT_COLORMAP_WORK) {
    __glutEstablishColormapsProperty(window);
  }
  if (workMask & GLUT_MAP_WORK) {
    switch (window->desiredMapState) {
    case WithdrawnState:
      if (window->parent) {
        XUnmapWindow(__glutDisplay, window->win);
      } else {
        XWithdrawWindow(__glutDisplay, window->win,
          __glutScreen);
      }
      break;
    case NormalState:
      XMapWindow(__glutDisplay, window->win);
      break;
    case IconicState:
      XIconifyWindow(__glutDisplay, window->win, __glutScreen);
      break;
    }
  }
  if (workMask & GLUT_REDISPLAY_WORK) {
    if (window->forceReshape) {
      /* Guarantee that before a display callback is generated
         for a window, a reshape callback must be generated. */
      __glutSetWindow(window);
      (*window->reshape) (window->width, window->height);
      window->forceReshape = False;
    }
    if (window->display) {
      __glutSetWindow(window);
      (*window->display) ();
    }
  }
  /* Combine workMask with window->workMask to determine what
     finish and debug work there is. */
  workMask |= window->workMask;

  if (workMask & GLUT_FINISH_WORK) {
    __glutSetWindow(window);
    glFinish();
  }
  if (workMask & GLUT_DEBUG_WORK) {
    GLenum error;

    __glutSetWindow(window);
    while ((error = glGetError()) != GL_NO_ERROR)
      __glutWarning("GL error: %s", gluErrorString(error));
  }
  /* Strip out dummy, finish, and debug work bits. */
  window->workMask &= ~(GLUT_DUMMY_WORK | GLUT_FINISH_WORK | GLUT_DEBUG_WORK);
  if (window->workMask) {
    /* Leave on work list. */
    return window;
  } else {
    /* Remove current window from work list. */
    return window->prevWorkWin;
  }
}
/* CENTRY */
void
glutMainLoop(void)
{
  if (!__glutDisplay)
    __glutFatalUsage("main loop entered with out X connection.");
  if (!__glutWindowListSize)
    __glutFatalUsage(
      "main loop entered with no windows created.");
  for (;;) {
    if (__glutWindowWorkList) {
      __glutWindowWorkList = processWindowWorkList(__glutWindowWorkList);
    }
    if (__glutIdleFunc || __glutWindowWorkList) {
      idleWait();
    } else {
      if (__glutTimerList) {
        waitForSomething();
      } else {
        processEvents();
      }
    }
  }
}
/* ENDCENTRY */
