/* Copyright (c) Mark J. Kilgard, 1994, 1995, 1996. */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>

#include <X11/Xlib.h>
#include <X11/cursorfont.h>  /* for XC_arrow */

#include <GL/glut.h>
#include "glutint.h"
#include "layerutil.h"

GLUTmenu *__glutCurrentMenu = NULL;
void (*__glutMenuStatusFunc) (int, int, int);
GLUTmenu *__glutMappedMenu;
GLUTwindow *__glutMenuWindow;
GLUTmenuItem *__glutItemSelected;

static GLUTmenu **menuList = NULL;
static int menuListSize = 0;
static XFontStruct *menuFont = NULL;
static Cursor menuCursor;
static Colormap menuColormap;
static Visual *menuVisual;
static int menuDepth;
static int fontHeight;
static GC blackGC, grayGC, whiteGC;
static unsigned long menuBlack, menuWhite, menuGray;

static void
menuVisualSetup(void)
{
  XLayerVisualInfo template, *visual, *overlayVisuals;
  XColor color;
  Status status;
  int nVisuals, i;

  template.layer = 1;
  template.vinfo.screen = __glutScreen;
  overlayVisuals = __glutXGetLayerVisualInfo(__glutDisplay,
    VisualScreenMask | VisualLayerMask, &template, &nVisuals);
  if (overlayVisuals) {
    for (i = 0; i < nVisuals; i++) {
      visual = &overlayVisuals[i];
      if (visual->vinfo.colormap_size >= 3) {
        menuColormap = XCreateColormap(__glutDisplay, __glutRoot,
          visual->vinfo.visual, AllocNone);
        /* Allocate overlay colormap cells in defined order:
           gray, black, white to match the IRIS GL allocation
           scheme.  Increases likelihood of less overlay
           colormap flashing. */
        /* XXX nice if these 3 AllocColor's could be done in
           one protocol round-trip */
        color.red = color.green = color.blue = 0xaa00;
        status = XAllocColor(__glutDisplay,
          menuColormap, &color);
        if (!status) {
          XFreeColormap(__glutDisplay, menuColormap);
          continue;
        }
        menuGray = color.pixel;
        color.red = color.green = color.blue = 0x0000;
        status = XAllocColor(__glutDisplay,
          menuColormap, &color);
        if (!status) {
          XFreeColormap(__glutDisplay, menuColormap);
          continue;
        }
        menuBlack = color.pixel;
        color.red = color.green = color.blue = 0xffff;
        status = XAllocColor(__glutDisplay,
          menuColormap, &color);
        if (!status) {
          XFreeColormap(__glutDisplay, menuColormap);
          continue;
        }
        menuWhite = color.pixel;
        menuVisual = visual->vinfo.visual;
        menuDepth = visual->vinfo.depth;
        XFree(overlayVisuals);
        return;
      }
    }
  }
  /* settle for default visual */
  menuVisual = DefaultVisual(__glutDisplay, __glutScreen);
  menuDepth = DefaultDepth(__glutDisplay, __glutScreen);
  menuColormap = DefaultColormap(__glutDisplay, __glutScreen);
  menuBlack = BlackPixel(__glutDisplay, __glutScreen);
  menuWhite = WhitePixel(__glutDisplay, __glutScreen);
  color.red = color.green = color.blue = 0xaa00;
  status = XAllocColor(__glutDisplay, menuColormap, &color);
  if (!status) {
    __glutFatalError(
      "could not allocate gray in default colormap");
  }
  menuGray = color.pixel;
  XFree(overlayVisuals);
}

static void
menuSetup(void)
{
  if (menuFont) {
    /* menuFont overload to indicate menu initalization */
    return;
  }
  menuFont = XLoadQueryFont(__glutDisplay,
    "-*-helvetica-bold-o-normal--14-*-*-*-p-*-iso8859-1");
  if (!menuFont) {
    /* try back up font */
    menuFont = XLoadQueryFont(__glutDisplay, "fixed");
  }
  if (!menuFont) {
    __glutFatalError("could not load font.");
  }
  menuVisualSetup();
  fontHeight = menuFont->ascent + menuFont->descent;
  menuCursor = XCreateFontCursor(__glutDisplay, XC_arrow);
}

static void
menuGraphicsContextSetup(Window win)
{
  XGCValues gcvals;

  if (blackGC != None)
    return;
  gcvals.font = menuFont->fid;
  gcvals.foreground = menuBlack;
  blackGC = XCreateGC(__glutDisplay, win,
    GCFont | GCForeground, &gcvals);
  gcvals.foreground = menuGray;
  grayGC = XCreateGC(__glutDisplay, win, GCForeground, &gcvals);
  gcvals.foreground = menuWhite;
  whiteGC = XCreateGC(__glutDisplay, win, GCForeground, &gcvals);
}

/* OBSOLETE - use glutMenuStatusFunc instead. */
void
glutMenuStateFunc(GLUTmenuStateCB menuStateFunc)
{
  __glutMenuStatusFunc = (GLUTmenuStatusCB) menuStateFunc;
}

void
glutMenuStatusFunc(GLUTmenuStatusCB menuStatusFunc)
{
  __glutMenuStatusFunc = menuStatusFunc;
}

void
__glutSetMenu(GLUTmenu * menu)
{
  __glutCurrentMenu = menu;
}

static void
unmapMenu(GLUTmenu * menu)
{
  if (menu->cascade) {
    unmapMenu(menu->cascade);
    menu->cascade = NULL;
  }
  menu->anchor = NULL;
  menu->highlighted = NULL;
  XUnmapWindow(__glutDisplay, menu->win);
}

void
__glutFinishMenu(Window win, int x, int y)
{
  Window dummy;
  int rc;

  unmapMenu(__glutMappedMenu);
  XUngrabPointer(__glutDisplay, CurrentTime);

  /* This XFlush is needed to to make sure the pointer is
     really ungrabbed when the application's menu callback is
     called. Otherwise, a deadlock might happen because the
     application may try to read from an terminal window, but
     yet the ungrab hasn't really happened since it hasn't been
     flushed out. */
  XFlush(__glutDisplay);

  if (__glutMenuStatusFunc) {
    if (win != __glutMenuWindow->win) {
      /* The button release may have occurred in a window other
         than the window requesting the pop-up menu (for
         example, one of the submenu windows).  In this case, we
         need to translate the coordinates into the coordinate
         system of the window associated with the window. */
      rc = XTranslateCoordinates(__glutDisplay, win, __glutMenuWindow->win,
        x, y, &x, &y, &dummy);
      assert(rc != False);  /* Will always be on same screen. */
    }
    __glutSetWindow(__glutMenuWindow);
    __glutSetMenu(__glutMappedMenu);

    /* Setting __glutMappedMenu to NULL permits operations that
       change menus or destroy the menu window again. */
    __glutMappedMenu = NULL;

    (*__glutMenuStatusFunc) (GLUT_MENU_NOT_IN_USE, x, y);
  }

  /* Setting __glutMappedMenu to NULL permits operations that
     change menus or destroy the menu window again. */
  __glutMappedMenu = NULL;

  /* If an item is selected and it is not a submenu trigger,
     generate menu callback. */
  if (__glutItemSelected && !__glutItemSelected->isTrigger) {
    __glutSetWindow(__glutMenuWindow);
    /* When menu callback is triggered, current menu should be
       set to the callback menu. */
    __glutSetMenu(__glutItemSelected->menu);
    /* Setting __glutMappedMenu to NULL permits operations that
       change menus or destroy the menu window again. */
    (*__glutItemSelected->menu->select) (
      __glutItemSelected->value);
  }
  __glutMenuWindow = NULL;
}

#define MENU_BORDER 1
#define MENU_GAP 2
#define MENU_ARROW_GAP 6
#define MENU_ARROW_WIDTH 8

static void
mapMenu(GLUTmenu * menu, int x, int y)
{
  XWindowChanges changes;
  unsigned int mask;
  int subMenuExtension, num;

  /* If there are submenus, we need to provide extra space for
     the submenu pull arrow.  */
  if (menu->submenus > 0) {
    subMenuExtension = MENU_ARROW_GAP + MENU_ARROW_WIDTH;
  } else {
    subMenuExtension = 0;
  }

  changes.stack_mode = Above;
  mask = CWStackMode | CWX | CWY;
  /* If the menu isn't managed (ie, validated so all the
     InputOnly subwindows are the right size), do so.  */
  if (!menu->managed) {
    GLUTmenuItem *item;

    item = menu->list;
    num = menu->num;
    while (item) {
      XWindowChanges itemupdate;

      itemupdate.y = (num - 1) * fontHeight + MENU_GAP;
      itemupdate.width = menu->pixwidth;
      itemupdate.width += subMenuExtension;
      XConfigureWindow(__glutDisplay, item->win,
        CWWidth | CWY, &itemupdate);
      item = item->next;
      num--;
    }
    menu->pixheight = MENU_GAP +
      fontHeight * menu->num + MENU_GAP;
    changes.height = menu->pixheight;
    changes.width = MENU_GAP +
      menu->pixwidth + subMenuExtension + MENU_GAP;
    mask |= CWWidth | CWHeight;
    menu->managed = True;
  }
  /* make sure menu appears fully on screen */
  if (y + menu->pixheight >= __glutScreenHeight) {
    changes.y = __glutScreenHeight - menu->pixheight;
  } else {
    changes.y = y;
  }
  if (x + menu->pixwidth + subMenuExtension >=
    __glutScreenWidth) {
    changes.x = __glutScreenWidth -
      menu->pixwidth + subMenuExtension;
  } else {
    changes.x = x;
  }

  /* Rember where the menu is placed so submenus can be
     properly placed relative to it. */
  menu->x = changes.x;
  menu->y = changes.y;

  XConfigureWindow(__glutDisplay, menu->win, mask, &changes);
  XInstallColormap(__glutDisplay, menuColormap);
  XMapWindow(__glutDisplay, menu->win);
}

void
__glutStartMenu(GLUTmenu * menu, GLUTwindow * window,
  int x, int y, int x_win, int y_win)
{
  int grab;

  assert(__glutMappedMenu == NULL);
  grab = XGrabPointer(__glutDisplay, __glutRoot, True,
    ButtonPressMask | ButtonReleaseMask,
    GrabModeAsync, GrabModeAsync,
    __glutRoot, menuCursor, CurrentTime);
  if (grab != GrabSuccess) {
    /* Somebody else has pointer grabbed, ignore menu
       activation. */
    return;
  }
  __glutMappedMenu = menu;
  __glutMenuWindow = window;
  __glutItemSelected = NULL;
  if (__glutMenuStatusFunc) {
    __glutSetMenu(menu);
    __glutSetWindow(window);
    (*__glutMenuStatusFunc) (GLUT_MENU_IN_USE, x_win, y_win);
  }
  mapMenu(menu, x, y);
}

static void
paintSubMenuArrow(Window win, int x, int y)
{
  XPoint p[5];

  p[0].x = p[4].x = x;
  p[0].y = p[4].y = y - menuFont->ascent + 1;
  p[1].x = p[0].x + MENU_ARROW_WIDTH - 1;
  p[1].y = p[0].y + (menuFont->ascent / 2) - 1;
  p[2].x = p[1].x;
  p[2].y = p[1].y + 1;
  p[3].x = p[0].x;
  p[3].y = p[0].y + menuFont->ascent - 2;
  XFillPolygon(__glutDisplay, win,
    whiteGC, p, 4, Convex, CoordModeOrigin);
  XDrawLines(__glutDisplay, win, blackGC, p, 5, CoordModeOrigin);
}

static void
paintMenuItem(GLUTmenuItem * item, int num)
{
  Window win = item->menu->win;
  GC gc;
  int y;
  int subMenuExtension;

  if (item->menu->submenus > 0) {
    subMenuExtension = MENU_ARROW_GAP + MENU_ARROW_WIDTH;
  } else {
    subMenuExtension = 0;
  }
  if (item->menu->highlighted == item) {
    gc = whiteGC;
  } else {
    gc = grayGC;
  }
  y = MENU_GAP + fontHeight * num - menuFont->descent;
  XFillRectangle(__glutDisplay, win, gc,
    MENU_GAP, y - fontHeight + menuFont->descent,
    item->menu->pixwidth + subMenuExtension, fontHeight);
  XDrawString(__glutDisplay, win, blackGC,
    MENU_GAP, y, item->label, item->len);
  if (item->isTrigger) {
    paintSubMenuArrow(win,
      item->menu->pixwidth + MENU_ARROW_GAP + 1, y);
  }
}

void
__glutPaintMenu(GLUTmenu * menu)
{
  GLUTmenuItem *item;
  int i = menu->num;
  int y = MENU_GAP + fontHeight * i - menuFont->descent;

  item = menu->list;
  while (item) {
    if (item->menu->highlighted == item) {
      paintMenuItem(item, i);
    } else {
      /* quick render of the menu item; assume background
         already cleared to gray */
      XDrawString(__glutDisplay, menu->win, blackGC,
        2, y, item->label, item->len);
      if (item->isTrigger) {
        paintSubMenuArrow(menu->win,
          menu->pixwidth + MENU_ARROW_GAP + 1, y);
      }
    }
    i--;
    y -= fontHeight;
    item = item->next;
  }
}

GLUTmenuItem *
__glutGetMenuItem(GLUTmenu * menu, Window win, int *which)
{
  GLUTmenuItem *item;
  int i;

  i = menu->num;
  item = menu->list;
  while (item) {
    if (item->win == win) {
      *which = i;
      return item;
    }
    if (item->isTrigger) {
      GLUTmenuItem *subitem;

      subitem = __glutGetMenuItem(menuList[item->value],
        win, which);
      if (subitem) {
        return subitem;
      }
    }
    i--;
    item = item->next;
  }
  return NULL;
}

static int
getMenuItemIndex(GLUTmenuItem * item)
{
  int count = 0;

  while (item) {
    count++;
    item = item->next;
  }
  return count;
}

GLUTmenu *
__glutGetMenu(Window win)
{
  GLUTmenu *menu;

  menu = __glutMappedMenu;
  while (menu) {
    if (win == menu->win) {
      return menu;
    }
    menu = menu->cascade;
  }
  return NULL;
}

GLUTmenu *
__glutGetMenuByNum(int menunum)
{
  if (menunum < 1 || menunum > menuListSize) {
    return NULL;
  }
  return menuList[menunum - 1];
}

static int
getUnusedMenuSlot(void)
{
  int i;

  /* Look for allocated, unused slot. */
  for (i = 0; i < menuListSize; i++) {
    if (!menuList[i]) {
      return i;
    }
  }
  /* Allocate a new slot. */
  menuListSize++;
  if (menuList) {
    menuList = (GLUTmenu **)
      realloc(menuList, menuListSize * sizeof(GLUTmenu *));
  } else {
    /* XXX Some realloc's do not correctly perform a malloc
       when asked to perform a realloc on a NULL pointer,
       though the ANSI C library spec requires this. */
    menuList = (GLUTmenu **) malloc(sizeof(GLUTmenu *));
  }
  if (!menuList)
    __glutFatalError("out of memory.");
  menuList[menuListSize - 1] = NULL;
  return menuListSize - 1;
}

int
glutCreateMenu(GLUTselectCB selectFunc)
{
  XSetWindowAttributes wa;
  GLUTmenu *menu;
  int menuid;

  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  if (!__glutDisplay)
    __glutOpenXConnection(NULL);
  menuid = getUnusedMenuSlot();
  menu = (GLUTmenu *) malloc(sizeof(GLUTmenu));
  if (!menu)
    __glutFatalError("out of memory.");
  menu->id = menuid;
  menu->num = 0;
  menu->submenus = 0;
  menu->managed = False;
  menu->pixwidth = 0;
  menu->select = selectFunc;
  menu->list = NULL;
  menu->cascade = NULL;
  menu->highlighted = NULL;
  menu->anchor = NULL;
  menuSetup();
  wa.override_redirect = True;
  wa.background_pixel = menuGray;
  wa.border_pixel = menuBlack;
  wa.colormap = menuColormap;
  wa.event_mask = StructureNotifyMask | ExposureMask |
    ButtonPressMask | ButtonReleaseMask |
    EnterWindowMask | LeaveWindowMask;
  menu->win = XCreateWindow(__glutDisplay, __glutRoot,
  /* real position determined when mapped */
    0, 0,
  /* real size will be determined when menu is manged */
    1, 1,
    MENU_BORDER, menuDepth, InputOutput, menuVisual,
    CWOverrideRedirect | CWBackPixel |
    CWBorderPixel | CWEventMask | CWColormap,
    &wa);
  menuGraphicsContextSetup(menu->win);
  menuList[menuid] = menu;
  __glutSetMenu(menu);
  return menuid + 1;
}

/* CENTRY */
void
glutDestroyMenu(int menunum)
{
  GLUTmenu *menu = __glutGetMenuByNum(menunum);
  GLUTmenuItem *item, *next;

  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  assert(menu->id == menunum - 1);
  XDestroySubwindows(__glutDisplay, menu->win);
  XDestroyWindow(__glutDisplay, menu->win);
  menuList[menunum - 1] = NULL;
  /* free all menu entries */
  item = menu->list;
  while (item) {
    assert(item->menu == menu);
    next = item->next;
    free(item->label);
    free(item);
    item = next;
  }
  if (__glutCurrentMenu == menu) {
    __glutCurrentMenu = NULL;
  }
  free(menu);
}

int
glutGetMenu(void)
{
  if (__glutCurrentMenu) {
    return __glutCurrentMenu->id + 1;
  } else {
    return 0;
  }
}

void
glutSetMenu(int menuid)
{
  GLUTmenu *menu;

  if (menuid < 1 || menuid > menuListSize) {
    __glutWarning("glutSetMenu attempted on bogus menu.");
    return;
  }
  menu = menuList[menuid - 1];
  if (!menu) {
    __glutWarning("glutSetMenu attempted on bogus menu.");
    return;
  }
  __glutSetMenu(menu);
}
/* ENDCENTRY */

static void
setMenuItem(GLUTmenuItem * item, char *label,
  int value, Bool isTrigger)
{
  GLUTmenu *menu;

  menu = item->menu;
  item->label = strdup(label);
  if (!item->label)
    __glutFatalError("out of memory.");
  item->isTrigger = isTrigger;
  item->len = (int) strlen(label);
  item->value = value;
  item->pixwidth = XTextWidth(menuFont, label, item->len) + 4;
  if (item->pixwidth > menu->pixwidth) {
    menu->pixwidth = item->pixwidth;
  }
  menu->managed = False;
}

/* CENTRY */
void
glutAddMenuEntry(char *label, int value)
{
  XSetWindowAttributes wa;
  GLUTmenuItem *entry;

  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  entry = (GLUTmenuItem *) malloc(sizeof(GLUTmenuItem));
  if (!entry)
    __glutFatalError("out of memory.");
  entry->menu = __glutCurrentMenu;
  setMenuItem(entry, label, value, False);
  wa.event_mask = EnterWindowMask | LeaveWindowMask;
  entry->win = XCreateWindow(__glutDisplay,
    __glutCurrentMenu->win, MENU_GAP,
    __glutCurrentMenu->num * fontHeight + MENU_GAP,  /* x & y */
    entry->pixwidth, fontHeight,  /* width & height */
    0, CopyFromParent, InputOnly, CopyFromParent,
    CWEventMask, &wa);
  XMapWindow(__glutDisplay, entry->win);
  __glutCurrentMenu->num++;
  entry->next = __glutCurrentMenu->list;
  __glutCurrentMenu->list = entry;
}

void
glutAddSubMenu(char *label, int menu)
{
  XSetWindowAttributes wa;
  GLUTmenuItem *submenu;

  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  submenu = (GLUTmenuItem *) malloc(sizeof(GLUTmenuItem));
  if (!submenu)
    __glutFatalError("out of memory.");
  __glutCurrentMenu->submenus++;
  submenu->menu = __glutCurrentMenu;
  setMenuItem(submenu, label, /* base 0 */ menu - 1, True);
  wa.event_mask = EnterWindowMask | LeaveWindowMask;
  submenu->win = XCreateWindow(__glutDisplay,
    __glutCurrentMenu->win, MENU_GAP,
    __glutCurrentMenu->num * fontHeight + MENU_GAP,  /* x & y */
    submenu->pixwidth, fontHeight,  /* width & height */
    0, CopyFromParent, InputOnly, CopyFromParent,
    CWEventMask, &wa);
  XMapWindow(__glutDisplay, submenu->win);
  __glutCurrentMenu->num++;
  submenu->next = __glutCurrentMenu->list;
  __glutCurrentMenu->list = submenu;
}

void
glutChangeToMenuEntry(int num, char *label, int value)
{
  GLUTmenuItem *item;
  int i;

  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  i = __glutCurrentMenu->num;
  item = __glutCurrentMenu->list;
  while (item) {
    if (i == num) {
      if (item->isTrigger) {
        /* If changing a submenu trigger to a menu entry, we
           need to account for submenus.  */
        item->menu->submenus--;
      }
      free(item->label);
      setMenuItem(item, label, value, False);
      return;
    }
    i--;
    item = item->next;
  }
  __glutWarning("Current menu has no %d item.", num);
}

void
glutChangeToSubMenu(int num, char *label, int menu)
{
  GLUTmenuItem *item;
  int i;

  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  i = __glutCurrentMenu->num;
  item = __glutCurrentMenu->list;
  while (item) {
    if (i == num) {
      if (!item->isTrigger) {
        /* If changing a menu entry to as submenu trigger, we
           need to account for submenus.  */
        item->menu->submenus++;
      }
      free(item->label);
      setMenuItem(item, label, /* base 0 */ menu - 1, True);
      return;
    }
    i--;
    item = item->next;
  }
  __glutWarning("Current menu has no %d item.", num);
}

void
glutRemoveMenuItem(int num)
{
  GLUTmenuItem *item, **prev, *remaining;
  int pixwidth, i;

  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  i = __glutCurrentMenu->num;
  prev = &__glutCurrentMenu->list;
  item = __glutCurrentMenu->list;
  /* If menu item is removed, the menu's pixwidth may need to
     be recomputed. */
  pixwidth = 0;
  while (item) {
    if (i == num) {
      /* If this menu item's pixwidth is as wide as the menu's
         pixwidth, removing this menu item will necessitate
         shrinking the menu's pixwidth. */
      if (item->pixwidth >= __glutCurrentMenu->pixwidth) {
        /* Continue recalculating menu pixwidth, first skipping
           the removed item. */
        remaining = item->next;
        while (remaining) {
          if (remaining->pixwidth > pixwidth) {
            pixwidth = remaining->pixwidth;
          }
          remaining = remaining->next;
        }
      }
      __glutCurrentMenu->num--;
      __glutCurrentMenu->managed = False;
      __glutCurrentMenu->pixwidth = pixwidth;

      /* Patch up menu's item list. */
      *prev = item->next;

      free(item->label);
      free(item);
      return;
    }
    if (item->pixwidth > pixwidth) {
      pixwidth = item->pixwidth;
    }
    i--;
    prev = &item->next;
    item = item->next;
  }
  __glutWarning("Current menu has no %d item.", num);
}

void
glutAttachMenu(int button)
{
  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  if (__glutCurrentWindow->menu[button] < 1) {
    __glutCurrentWindow->buttonUses++;
  }
  __glutChangeWindowEventMask(
    ButtonPressMask | ButtonReleaseMask, True);
  __glutCurrentWindow->menu[button] = __glutCurrentMenu->id + 1;
}

void
glutDetachMenu(int button)
{
  if (__glutMappedMenu) {
    __glutFatalUsage("menu manipulation not allowed while menus in use");
  }
  if (__glutCurrentWindow->menu[button] > 0) {
    __glutCurrentWindow->buttonUses--;
    __glutChangeWindowEventMask(ButtonPressMask | ButtonReleaseMask,
      __glutCurrentWindow->buttonUses > 0);
    __glutCurrentWindow->menu[button] = 0;
  }
}
/* ENDCENTRY */

void
__glutMenuItemEnterOrLeave(GLUTmenuItem * item,
  int num, int type)
{
  int alreadyUp = 0;

  if (type == EnterNotify) {
    GLUTmenuItem *prevItem = item->menu->highlighted;

    if (prevItem && prevItem != item) {
      /* If there's an already higlighted item in this menu
         that is different from this one (we could be
         re-entering an item with an already cascaded
         submenu!), unhighlight the previous item. */
      item->menu->highlighted = NULL;
      paintMenuItem(prevItem, getMenuItemIndex(prevItem));
    }
    item->menu->highlighted = item;
    __glutItemSelected = item;
    if (item->menu->cascade) {
      if (!item->isTrigger) {
        /* Entered a menu item that is not a submenu trigger,
           so pop down the current submenu cascade of this
           menu.  */
        unmapMenu(item->menu->cascade);
        item->menu->cascade = NULL;
      } else {
        GLUTmenu *submenu = menuList[item->value];

        if (submenu->anchor == item) {
          /* We entered the submenu trigger for the submenu
             that is already up, so don't take down the
             submenu.  */
          alreadyUp = 1;
        } else {
          /* Submenu already popped up for some other submenu
             item of this menu; need to pop down that other
             submenu cascade.  */
          unmapMenu(item->menu->cascade);
          item->menu->cascade = NULL;
        }
      }
    }
    if (!alreadyUp) {
      /* Make sure the menu item gets painted with
         highlighting. */
      paintMenuItem(item, num);
    } else {
      /* If already up, should already be highlighted.  */
    }
  } else {
    /* LeaveNotify: Handle leaving a menu item...  */
    if (item->menu->cascade &&
      item->menu->cascade->anchor == item) {
      /* If there is a submenu casacaded from this item, do not 

         change the highlighting on this item upon leaving. */
    } else {
      /* Unhighlight this menu item.  */
      item->menu->highlighted = NULL;
      paintMenuItem(item, num);
    }
    __glutItemSelected = NULL;
  }
  if (item->isTrigger) {
    if (type == EnterNotify && !alreadyUp) {
      GLUTmenu *submenu = menuList[item->value];

      mapMenu(submenu,
        item->menu->x + item->menu->pixwidth +
        MENU_ARROW_GAP + MENU_ARROW_WIDTH +
        MENU_GAP + MENU_BORDER,
        item->menu->y + fontHeight * (num - 1) + MENU_GAP);
      item->menu->cascade = submenu;
      submenu->anchor = item;
    }
  }
}
