/* $XConsortium: XKBlib.h,v 1.8 94/04/08 15:11:49 erik Exp $ */
/************************************************************
Copyright (c) 1993 by Silicon Graphics Computer Systems, Inc.

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of Silicon Graphics not be 
used in advertising or publicity pertaining to distribution 
of the software without specific prior written permission.
Silicon Graphics makes no representation about the suitability 
of this software for any purpose. It is provided "as is"
without any express or implied warranty.

SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS 
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY 
AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, 
DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

#ifndef _XKBLIB_H_
#define _XKBLIB_H_

#include <X11/extensions/XKBstr.h>

typedef struct _XkbAnyEvent {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* # of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XKB event minor code */
	unsigned int 	device;		/* device ID */
} XkbAnyEvent;

typedef struct _XkbNewKeyboardNotify {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbNewKeyboardNotify */
	int	 	device;		/* device ID */
	int	 	min_key_code;	/* minimum key code */
	int		max_key_code;	/* maximum key code */
	int	 	old_device;	/* device ID of previous keyboard */
	int	 	old_min_key_code;/* min key code of previous kbd */
	int		old_max_key_code;/* max key code of previous kbd */
	KeyCode	 	keycode;	/* key that caused change or 0 */
	char	 	event_type;	/* type of event that caused change */
	char	 	req_major;	/* if keycode==0, major and minor */
	char	 	req_minor;	/* opcode of req that caused change */
} XkbNewKeyboardNotifyEvent;

typedef struct _XkbMapNotifyEvent {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbMapNotify */
	int 		device;		/* device ID */
	unsigned int 	changed;	/* fields which have been changed */
	unsigned int 	resized;	/* fields which have been resized */
	int 		first_type;	/* first changed key type */
	int 		num_types;	/* number of changed key types */
	KeyCode		min_key_code;
	KeyCode		max_key_code;
	KeyCode		first_key_sym;
	KeyCode		first_key_act;
	KeyCode		first_key_behavior;
	KeyCode		first_key_explicit;
	KeyCode		first_modmap_key;  
	KeyCode		first_vmodmap_key;
	int		num_key_syms;
	int		num_key_acts;
	int		num_key_behaviors;
	int		num_key_explicit;
	int 		num_modmap_keys;
	int 		num_vmodmap_keys;
	unsigned int 	vmods;		/* mask of changed virtual mods */
} XkbMapNotifyEvent;

typedef struct _XkbStateNotifyEvent {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* # of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbStateNotify */
	int 		device;		/* device ID */
	unsigned int 	changed;	/* mask of changed state components */
	int 		group;		/* keyboard group */
	int 		base_group;	/* base keyboard group */
	int 		latched_group;	/* latched keyboard group */
	int 		locked_group;	/* locked keyboard group */
	unsigned int	mods;		/* modifier state */
	unsigned int 	base_mods;	/* base modifier state */
	unsigned int	latched_mods;	/* latched modifiers */
	unsigned int	locked_mods;	/* locked modifiers */
	int 		compat_state;	/* compatibility state */
	unsigned char	grab_mods;	/* mods used for grabs */
	unsigned char	compat_grab_mods;/* grab mods for non-XKB clients */
	unsigned char	lookup_mods;	/* mods sent to clients */
	unsigned char	compat_lookup_mods; /* mods sent to non-XKB clients */
	int 		ptr_buttons;	/* pointer button state */
	KeyCode		keycode;	/* keycode that caused the change */
	char 		event_type;	/* KeyPress or KeyRelease */
	char 		req_major;	/* Major opcode of request */
	char 		req_minor;	/* Minor opcode of request */
} XkbStateNotifyEvent;

typedef struct _XkbControlsNotify {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbControlsNotify */
	int 		device;		/* device ID */
	unsigned int	changed_ctrls;	/* controls with changed sub-values */
	unsigned int 	enabled_ctrls;	/* controls currently enabled */
	unsigned int	enabled_ctrl_changes;/* controls just {en,dis}abled */
	int 		num_groups;	/* total groups on keyboard */
	KeyCode		keycode;	/* key that caused change or 0 */
	char 		event_type;	/* type of event that caused change */
	char 		req_major;	/* if keycode==0, major and minor */
	char 		req_minor;	/* opcode of req that caused change */
} XkbControlsNotifyEvent;

typedef struct _XkbIndicatorNotify {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbIndicatorNotify */
	int 		device;		/* device ID */
	unsigned int	state_changed;	/* indicators that have changed state */
	unsigned int	state;	 	/* current state of all indicators */
	unsigned int 	map_changed;	/* indicators whose maps have changed */
} XkbIndicatorNotifyEvent;

typedef struct _XkbNamesNotify {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbNamesNotify */
	int	 	device;		/* device ID */
	unsigned int 	changed;	/* names that have changed */
	int	 	first_type;	/* first key type with new name */
	int	 	num_types;	/* number of key types with new names */
	int	 	first_lvl;	/* first key type new new level names */
	int	 	num_lvls;	/* # of key types w/new level names */
	int	 	first_radio_group;/* first radio group with new name */
	int	 	num_radio_groups;/* # of radio groups with new names */
	unsigned int 	changed_vmods;	/* virtual modifiers with new names */
	unsigned int 	changed_groups;	/* groups with new names */
	unsigned int 	changed_indicators;/* indicators with new names */
} XkbNamesNotifyEvent;

typedef struct _XkbCompatMapNotify {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbCompatMapNotify */
	int	 	device;		/* device ID */
	unsigned int 	changed_groups; /* groups with new compat maps */
	int	 	first_si;	/* first new symbol interp */
	int	 	num_si;		/* number of new symbol interps */
	int	 	num_total_si;	/* total # of symbol interps */
} XkbCompatMapNotifyEvent;

typedef struct _XkbBellNotify {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbBellNotify */
	int	 	device;		/* device ID */
	int	 	percent;	/* requested volume as a % of maximum */
	int	 	pitch;		/* requested pitch in Hz */
	int	 	duration;	/* requested duration in useconds */
	int	 	bell_class;	/* (input extension) feedback class */
	int	 	bell_id;	/* (input extension) ID of feedback */
	Atom 		name;		/* "name" of requested bell */
	Window 		window;		/* window associated with event */
	Bool		event_only;	/* "event only" requested */
} XkbBellNotifyEvent;

typedef struct _XkbActionMessage {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbActionMessage */
	int	 	device;		/* device ID */
	KeyCode		keycode;	/* key that generated the event */
	Bool 		press;		/* true if act caused by key press */
	Bool 		key_event_follows;/* true if key event also generated */
	char 		message[XkbActionMessageLength+1]; 
					/* message -- leave space for NUL */
} XkbActionMessageEvent;

typedef struct _XkbSlowKeyNotify {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbSlowKeyNotify */
	int	 	device;		/* device ID */
	int	 	slow_key_state;	/* press, release, accept, reject */
	int	 	keycode;	/* key of event */
	int	 	delay;		/* current delay in milliseconds */
} XkbSlowKeyNotifyEvent;

typedef struct _XkbButtonActionsNotify {
	int 		type;		/* XkbAnyEvent */
	unsigned long 	serial;		/* of last req processed by server */
	Bool 		send_event;	/* is this from a SendEvent request? */
	Display *	display;	/* Display the event was read from */
	Time 		time;		/* milliseconds */
	int 		xkb_type;	/* XkbButtonActionsNotify */
	int	 	device;		/* device ID */
	int	 	first_btn;	/* first button that changed */
	int	 	num_btns;	/* range of buttons changed */
	int	 	count;		/* number of events that follow */
	char		changed[16];	/* changes for 64 buttons after first */
} XkbButtonActionsNotifyEvent;

typedef union _XkbEvent {
	int				type;
	XkbAnyEvent			any;
	XkbNewKeyboardNotifyEvent	new_kbd;
	XkbMapNotifyEvent		map;
	XkbStateNotifyEvent		state;
	XkbControlsNotifyEvent		ctrls;
	XkbIndicatorNotifyEvent 	indicators;
	XkbNamesNotifyEvent		names;
	XkbCompatMapNotifyEvent		compat;
	XkbBellNotifyEvent		bell;
	XkbActionMessageEvent		message;
	XkbSlowKeyNotifyEvent		slow_key;
	XkbButtonActionsNotifyEvent 	btn_acts;
	XEvent				core;
} XkbEvent;

typedef struct	_XkbKbdDpyState	XkbKbdDpyStateRec,*XkbKbdDpyStatePtr;

	/* XkbOpenDisplay error codes */
#define	XkbOD_Success		0
#define	XkbOD_BadLibraryVersion	1
#define	XkbOD_ConnectionRefused	2
#define	XkbOD_NonXkbServer	3
#define	XkbOD_BadServerVersion	4

	/* Values for XlibFlags */
#define	XkbLC_ForceLatin1Lookup	(1<<0)
#define	XkbLC_ConsumeLookupMods	(1<<1)
#define	XkbLC_ComposeLED	(1<<30)
#define	XkbLC_BeepOnComposeFail	(1<<31)

#define	XkbLC_AllControls	(0xc0000003)

_XFUNCPROTOBEGIN

extern	Bool	XkbIgnoreExtension(
#if NeedFunctionPrototypes
	Bool			/* ignore */
#endif
);

extern	Display *XkbOpenDisplay(
#if NeedFunctionPrototypes
	char *			/* name */,
	int *			/* ev_rtrn */,
	int *			/* err_rtrn */,
	int *			/* major_rtrn */,
	int *			/* minor_rtrn */,
	int *			/* reason */
#endif
);

extern	Bool	XkbQueryExtension(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	int *			/* opcodeReturn */,
	int *			/* eventBaseReturn */,
	int *			/* errorBaseReturn */,
	int *			/* majorRtrn */,
	int *			/* minorRtrn */
#endif
);

extern	Bool	XkbUseExtension(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	int *			/* major_rtrn */,
	int *			/* minor_rtrn */
#endif
);

extern	Bool	XkbLibraryVersion(
#if NeedFunctionPrototypes
	int *			/* libMajorRtrn */,
	int *			/* libMinorRtrn */
#endif
);

extern	unsigned	XkbKeysymToModifiers(
#if NeedFunctionPrototypes
    Display *			/* dpy */,
    KeySym 			/* ks */
#endif
);

extern	int		XkbLookupKeySym(
#if NeedFunctionPrototypes
    Display *			/* dpy */,
    KeyCode 			/* keycode */,
    unsigned int 		/* modifiers */,
    unsigned int *		/* modifiers_return */,
    KeySym *			/* keysym_return */
#endif
);

extern	int		XkbLookupKeyBinding(
#if NeedFunctionPrototypes
    Display *			/* dpy */,
    KeySym *			/* sym_rtrn */,
    unsigned int 		/* mods */,
    char *			/* buffer */,
    int 			/* nbytes */,
    int * 			/* extra_rtrn */
#endif
);

extern	int		XkbTranslateKeyCode(
#if NeedFunctionPrototypes
    XkbDescPtr			/* xkb */,
    KeyCode 			/* keycode */,
    unsigned int 		/* modifiers */,
    unsigned int *		/* modifiers_return */,
    KeySym *			/* keysym_return */
#endif
);

extern	int		XkbTranslateKeySym(
#if NeedFunctionPrototypes
    Display *			/* dpy */,
    register KeySym *		/* sym_return */,
    unsigned int 		/* modifiers */,
    char *			/* buffer */,
    int 			/* nbytes */
#endif
);

extern	Bool	XkbSetAutoRepeatRate(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned		/* deviceSpec */,
	unsigned		/* delay */,
	unsigned		/* interval */
#endif
);

extern	Bool	XkbGetAutoRepeatRate(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned		/* deviceSpec */,
	unsigned *		/* delayRtrn */,
	unsigned *		/* intervalRtrn */
#endif
);

extern	Bool	XkbChangeEnabledControls(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned		/* deviceSpec */,
	unsigned		/* affect */,
	unsigned		/* values */
#endif
);

extern	Bool	XkbDeviceBell(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	Window			/* win */,
	int			/* deviceSpec */,
	int			/* bellClass */,
	int			/* bellID */,
	int			/* percent */,
	Atom			/* name */
#endif
);

extern	Bool	XkbForceDeviceBell(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	Window			/* win */,
	int			/* deviceSpec */,
	int			/* bellClass */,
	int			/* bellID */,
	int			/* percent */
#endif
);

extern	Bool	XkbDeviceBellEvent(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	Window			/* win */,
	int			/* deviceSpec */,
	int			/* bellClass */,
	int			/* bellID */,
	int			/* percent */,
	Atom			/* name */
#endif
);

extern	Bool	XkbBell(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	Window			/* win */,
	int			/* percent */,
	Atom			/* name */
#endif
);

extern	Bool	XkbForceBell(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	Window			/* win */,
	int			/* percent */
#endif
);

extern	Bool	XkbBellEvent(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	Window			/* win */,
	int			/* percent */,
	Atom			/* name */
#endif
);

extern	Bool	XkbSelectEvents(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned		/* deviceID */,
	unsigned long		/* affect */,
	unsigned long		/* values */
#endif
);

extern	Bool	XkbSelectEventDetails(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* deviceID */,
	unsigned 		/* eventType */,
	unsigned long 		/* affect */,
	unsigned long 		/* details */
#endif
);

extern	void	XkbNoteMapChanges(
#if NeedFunctionPrototypes
    XkbMapChangesPtr		/* old */,
    XkbMapNotifyEvent	*	/* new */,
    unsigned int	 	/* wanted */
#endif
);

extern	Status	XkbGetIndicatorState(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned int 		/* deviceSpec */,
	unsigned int *		/* pStateRtrn */
#endif
);

extern	Status	 XkbGetIndicatorMap(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned long		/* which */,
	XkbDescPtr		/* desc */
#endif
);

extern	Bool	 XkbSetIndicatorMap(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned long 		/* which */,
	XkbDescPtr		/* desc */
#endif
);

extern	Bool	XkbGetNamedIndicator(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned int		/* deviceSpec */,
	Atom			/* name */,
	int *			/* pNdxRtrn */,
	Bool *			/* pStateRtrn */,
	XkbIndicatorMapPtr	/* pMapRtrn */,
	Bool *			/* pRealRtrn */
#endif
);

extern	Bool	XkbSetNamedIndicator(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned int		/* deviceSpec */,
	Atom			/* name */,
	Bool *			/* pState */,
	Bool			/* createNewMap */,
	XkbIndicatorMapPtr	/* pMap */
#endif
);

extern	Bool	XkbLockModifiers(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* deviceSpec */,
	unsigned 		/* affect */,
	unsigned 		/* values */
#endif
);

extern	Bool	XkbLatchModifiers(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* deviceSpec */,
	unsigned 		/* affect */,
	unsigned 		/* values */
#endif
);

extern	Bool	XkbLockGroup(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* deviceSpec */,
	unsigned 		/* group */
#endif
);

extern	Bool	XkbLatchGroup(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* deviceSpec */,
	unsigned 		/* group */
#endif
);

extern	Bool	XkbSetServerInternalMods(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* deviceSpec */,
	unsigned 		/* affectReal */,
	unsigned 		/* realValues */,
	unsigned		/* affectVirtual */,
	unsigned		/* virtualValues */
#endif
);

extern	Bool	XkbSetIgnoreLockMods(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* deviceSpec */,
	unsigned 		/* affectReal */,
	unsigned 		/* realValues */,
	unsigned		/* affectVirtual */,
	unsigned		/* virtualValues */
#endif
);


extern	Bool	XkbVirtualModsToReal(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	unsigned		/* virtual_mask */,
	unsigned *		/* mask_rtrn */
#endif
);

extern	Bool	XkbComputeEffectiveMap(
#if NeedFunctionPrototypes
	XkbDescPtr 		/* xkb */,
	XkbKeyTypePtr		/* type */,
	unsigned char *		/* map_rtrn */
#endif
);

extern	Status XkbInitCanonicalKeyTypes(
#if NeedFunctionPrototypes
    XkbDescPtr			/* xkb */,
    unsigned			/* which */,
    int				/* keypadVMod */
#endif
);

extern	XkbDescRec *XkbAllocKeyboard(
#if NeedFunctionPrototypes
	void
#endif
);

extern	void	XkbFreeKeyboard(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	unsigned		/* which */,
	Bool			/* freeDesc */
#endif
);

extern	Status XkbAllocClientMap(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	unsigned		/* which */,
	unsigned		/* nTypes */
#endif
);

extern	Status XkbAllocServerMap(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	unsigned		/* which */,
	unsigned		/* nActions */
#endif
);

extern	void	XkbFreeClientMap(
#if NeedFunctionPrototypes
    XkbDescPtr			/* xkb */,
    unsigned			/* what */,
    Bool			/* freeMap */
#endif
);

extern	void	XkbFreeServerMap(
#if NeedFunctionPrototypes
    XkbDescPtr			/* xkb */,
    unsigned			/* what */,
    Bool			/* freeMap */
#endif
);

extern	Status XkbAllocIndicatorMaps(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */
#endif
);

extern	void XkbFreeIndicatorMaps(
#if NeedFunctionPrototypes
    XkbDescPtr			/* xkb */
#endif
);

extern	XkbDescPtr XkbGetMap(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* which */,
	unsigned 		/* deviceSpec */
#endif
);

extern	Status	XkbGetUpdatedMap(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* which */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbGetMapChanges(
#if NeedFunctionPrototypes
    Display *			/* dpy */,
    XkbDescRec *		/* xkb */,
    XkbMapChangesRec *		/* changes */
#endif
);

extern	Status	XkbRefreshKeyboardMapping(
#if NeedFunctionPrototypes
    XkbMapNotifyEvent *		/* event */
#endif
);

extern	Status	XkbGetActions(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* firstAction */,
	unsigned		/* nActions */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbGetKeySyms(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* firstKey */,
	unsigned		/* nKeys */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbGetKeyActions(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* firstKey */,
	unsigned		/* nKeys */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbGetKeyBehaviors(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* firstKey */,
	unsigned		/* nKeys */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbGetVirtualMods(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* which */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbGetKeyExplicitComponents(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* firstKey */,
	unsigned		/* nKeys */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbGetKeyModifierMap(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* firstKey */,
	unsigned		/* nKeys */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbAllocControls(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	unsigned		/* which*/
#endif
);

extern	void	XkbFreeControls(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	unsigned		/* which */,
	Bool			/* freeMap */
#endif
);

extern	Status	XkbGetControls(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned long		/* which */,
	XkbDescPtr		/* desc */
#endif
);

extern	Bool	XkbSetControls(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned long		/* which */,
	XkbDescPtr		/* desc */
#endif
);

extern	Status	XkbAllocCompatMap(
#if NeedFunctionPrototypes
    XkbDescPtr			/* xkb */,
    unsigned			/* which */,
    unsigned			/* nInterpret */
#endif
);

extern	void	XkbFreeCompatMap(
#if NeedFunctionPrototypes
    XkbDescPtr			/* xkb */,
    unsigned			/* which */,
    Bool			/* freeMap */
#endif
);

extern Status XkbGetCompatMap(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* which */,
	XkbDescPtr 		/* xkb */
#endif
);

extern Bool XkbSetCompatMap(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* which */,
	XkbDescPtr 		/* xkb */,
	Bool			/* updateActions */
#endif
);

extern	Status XkbAllocNames(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	unsigned		/* which */,
	int			/* nTotalRG */,
	int			/* nTotalAliases */
#endif
);

extern	Status	XkbGetNames(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned		/* which */,
	XkbDescPtr		/* desc */
#endif
);

extern	Bool	XkbSetNames(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned		/* which */,
	unsigned		/* firstType */,
	unsigned		/* nTypes */,
	XkbDescPtr		/* desc */
#endif
);

extern	Bool	XkbChangeNames(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	XkbDescPtr		/* xkb */,
	XkbNameChangesPtr	/* changes */
#endif
);

extern	void XkbFreeNames(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	unsigned		/* which */,
	Bool			/* freeMap */
#endif
);


extern	Status	XkbGetState(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* deviceSpec */,
	XkbStatePtr		/* rtrnState */
#endif
);

extern	Bool	XkbSetMap(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned		/* which */,
	XkbDescPtr		/* desc */
#endif
);

extern	Bool	XkbChangeMap(
#if NeedFunctionPrototypes
	Display*		/* dpy */,
	XkbDescPtr		/* desc */,
	XkbMapChangesPtr	/* changes */
#endif
);

extern	unsigned	XkbSetXlibControls(
#if NeedFunctionPrototypes
	Display*		/* dpy */,
	unsigned		/* affect */,
	unsigned		/* values */
#endif
);

extern	Bool	XkbSetDetectableAutoRepeat(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	Bool			/* detectable */,
	Bool *			/* supported */
#endif
);

extern	Bool	XkbGetDetectableAutoRepeat(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	Bool *			/* supported */
#endif
);

extern	Bool	XkbSetAutoResetControls(
#if NeedFunctionPrototypes
    Display *			/* dpy */,
    unsigned 			/* changes */,
    unsigned *			/* auto_ctrls */,
    unsigned *			/* auto_values */
#endif
);

extern	Bool	XkbGetAutoResetControls(
#if NeedFunctionPrototypes
    Display *			/* dpy */,
    unsigned *			/* auto_ctrls */,
    unsigned *			/* auto_ctrl_values */
#endif
);

extern	Status	XkbResizeKeyType(
#if NeedFunctionPrototypes
    XkbDescPtr		/* xkb */,
    int			/* type_ndx */,
    int			/* map_count */,
    Bool		/* want_preserve */,
    int			/* new_num_lvls */
#endif
);

extern	KeySym *XkbResizeKeySyms(
#if NeedFunctionPrototypes
	XkbDescPtr		/* desc */,
	int 			/* forKey */,
	int 			/* symsNeeded */
#endif
);

extern	XkbAction *XkbResizeKeyActions(
#if NeedFunctionPrototypes
	XkbDescPtr		/* desc */,
	int 			/* forKey */,
	int 			/* actsNeeded */
#endif
);

extern	Status XkbChangeTypesOfKey(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	int 			/* key */,
	int			/* num_groups */,
	unsigned		/* groups */,
	int *			/* newTypes */,
	XkbMapChangesRec *	/* pChanges */
#endif
);

extern	XkbDescPtr XkbGetKeyboard(
#if NeedFunctionPrototypes
	Display *		/* dpy */,
	unsigned 		/* which */,
	unsigned 		/* deviceSpec */
#endif
);

extern XkbDescPtr XkbGetKeyboardByName(
#if NeedFunctionPrototypes
    Display *			/* dpy */,
    unsigned			/* deviceSpec */,
    XkbComponentNamesPtr	/* names */,
    char **			/* compiled_name_rtrn */,
    unsigned 			/* want */,
    unsigned 			/* need */,
    Bool			/* load */
#endif
);

/***====================================================================***/

extern	int	XkbKeyTypesForCoreSymbols(	/* returns # of groups */
#if NeedFunctionPrototypes
    int		/* map_width */,		/* width of core KeySym array */
    KeySym *	/* syms_inout */,		/* always mapWidth symbols */
    int *	/* types_rtrn */		/* always two type indices */
#endif
);

extern	Bool	XkbApplyCompatMapToKey(	/* False only on error */
#if NeedFunctionPrototype
    XkbDescPtr		/* xkb */,		/* keymap to be edited */
    KeyCode		/* key */,		/* key to be updated */
    XkbMapChangesPtr	/* changes */		/* resulting changes to map */
#endif
);

extern	Bool	XkbUpdateMapFromCore( /* False only on error */
#if NeedFunctionPrototype
    XkbDescPtr		/* xkb */,		/* XKB keyboard to be edited */
    KeyCode		/* first_key */,	/* first changed key */
    int			/* num_keys */, 	/* number of changed keys */
    int			/* map_width */,	/* width of core keymap */
    KeySym *		/* core_keysyms */	/* symbols from core keymap */
#endif
);

/***====================================================================***/

extern	Bool XkbSetDebuggingFlags(
#if NeedFunctionPrototypes
    Display *		/* dpy */,
    unsigned int	/* mask */,
    unsigned int	/* flags */,
    char *		/* msg */,
    unsigned int *	/* rtrnFlags */,
    Bool *		/* disableLocks */
#endif
);

_XFUNCPROTOEND

#endif /* _XKBLIB_H_ */
