/* $XConsortium: XKBproto.h,v 1.13 94/04/02 14:19:54 erik Exp $ */
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

#ifndef _XKBPROTO_H_
#define	_XKBPROTO_H_

#include <X11/Xmd.h>
#include <X11/extensions/XKB.h>

#define Window CARD32
#define Atom CARD32
#define Time CARD32
#define KeyCode CARD8
#define KeySym CARD32

#define	XkbPaddedSize(n)	((((unsigned)(n)+3) >> 2) << 2)

typedef struct _xkbUseExtension {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBUseExtension */
    CARD16	length B16;
    CARD16	wantedMajor B16;
    CARD16	wantedMinor B16;
} xkbUseExtensionReq;
#define	sz_xkbUseExtensionReq	8

typedef struct _xkbUseExtensionReply {
    BYTE	type;		/* X_Reply */
    BOOL	supported;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD16	serverMajor B16;
    CARD16	serverMinor B16;
    CARD32	pad1 B32;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
} xkbUseExtensionReply;
#define	sz_xkbUseExtensionReply	32

typedef	struct _xkbSelectEvents {
    CARD8	reqType;
    CARD8	xkbReqType;	/* X_KBSelectEvents */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	affectWhich B16;
    CARD16	clear B16;
    CARD16	selectAll B16;
    CARD16	affectMap B16;
    CARD16	map B16;
} xkbSelectEventsReq;
#define	sz_xkbSelectEventsReq	16

#ifdef NEED_EVENTS

typedef struct _xkbSendEvent {
    CARD8	reqType;
    CARD8	xkbReqType;	/* X_KBSendEvent */
    CARD16	length B16;
    BOOL	propagate;
    BOOL	synthesizeClick;
    CARD16	pad B16;
    CARD32	destination B32;/* Window */
    CARD32	eventMask B32;
    xEvent	event;
} xkbSendEventReq;
#define	sz_xkbSendEventReq	48

#endif /* NEED_EVENTS */

typedef struct _xkbBell {
    CARD8	reqType;
    CARD8	xkbReqType;	/* X_KBBell */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD8	bellClass;
    CARD8	bellID;
    INT8	percent;
    BOOL	override;
    BOOL	eventOnly;
    CARD8	pad;
    CARD16	pitch B16;
    CARD16	duration B16;
    Atom	name B32;
    Window	window B32;
} xkbBellReq;
#define	sz_xkbBellReq		24

typedef struct _xkbGetNamedIndicator {
    CARD8	reqType;
    CARD8	xkbReqType;	/* X_KBGetNamedIndicator */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad1 B16;
    Atom	indicator B32;
} xkbGetNamedIndicatorReq;
#define	sz_xkbGetNamedIndicatorReq		12

typedef	struct _xkbGetNamedIndicatorReply {
    BYTE	type;
    BYTE	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    Atom	indicator B32;
    BOOL	found;
    BOOL	on;
    BOOL	realIndicator;
    CARD8	ndx;
    CARD8	flags;
    CARD8	whichGroups;
    CARD8	groups;
    CARD8	whichMods;
    CARD8	mods;
    CARD8	realMods;
    CARD16	virtualMods B16;
    CARD32	ctrls B32;
    CARD32	pad2 B32;
} xkbGetNamedIndicatorReply;
#define	sz_xkbGetNamedIndicatorReply	32

typedef struct _xkbSetNamedIndicator {
    CARD8	reqType;
    CARD8	xkbReqType;	/* X_KBSetNamedIndicator */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad1 B16;
    Atom	indicator B32;
    BOOL	setState;
    BOOL	on;
    BOOL	setMap;
    BOOL	createMap;
    CARD8	flags;
    CARD8	whichGroups;
    CARD8	groups;
    CARD8	whichMods;
    CARD16	virtualMods B16;
    CARD8	realMods;
    CARD8	pad2;
    CARD32	ctrls B32;
} xkbSetNamedIndicatorReq;
#define	sz_xkbSetNamedIndicatorReq	28

typedef struct _xkbGetState {
	CARD8		reqType;
	CARD8		xkbReqType;	/* always X_KBGetState */
	CARD16		length B16;
	CARD16		deviceSpec B16;
	CARD16		pad B16;
} xkbGetStateReq;
#define	sz_xkbGetStateReq	8

typedef	struct _xkbGetStateReply {
    BYTE	type;
    BYTE	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD8	mods;
    CARD8	baseMods;
    CARD8	latchedMods;
    CARD8	lockedMods;
    CARD8	group;
    CARD8	baseGroup;
    CARD8	latchedGroup;
    CARD8	lockedGroup;
    CARD8	compatState;
    CARD8	grabMods;
    CARD8	compatGrabMods;
    CARD8	lookupMods;
    CARD8	compatLookupMods;
    CARD8	pad1;
    CARD16	ptrBtnState B16;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
} xkbGetStateReply;
#define	sz_xkbGetStateReply	32

typedef struct _xkbLatchLockState {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBLatchLockState */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD8	affectModLocks;
    CARD8	modLocks;
    BOOL	lockGroup;
    CARD8	groupLock;
    CARD8	affectModLatches;
    CARD8	modLatches;
    BOOL	latchGroup;
    CARD8	groupLatch;
    CARD16	pad B16;
} xkbLatchLockStateReq;
#define	sz_xkbLatchLockStateReq		16

typedef struct _xkbGetControls {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetControls */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad B16;
} xkbGetControlsReq;
#define	sz_xkbGetControlsReq	8

typedef struct _xkbGetControlsReply {
    BYTE	type;		/* X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD8	mouseKeysDfltBtn;
    CARD8	numGroups;
    CARD8	groupsWrap;
    CARD8	internalMods;
    CARD8	ignoreLockMods;
    CARD8	internalRealMods;
    CARD8	ignoreLockRealMods;
    CARD8	pad1;
    CARD16	internalVirtualMods B16;
    CARD16	ignoreLockVirtualMods B16;
    CARD16	repeatDelay B16;
    CARD16	repeatInterval B16;
    CARD16	slowKeysDelay B16;
    CARD16	debounceDelay B16;
    CARD16	mouseKeysDelay B16;
    CARD16	mouseKeysInterval B16;
    CARD16	mouseKeysTimeToMax B16;
    CARD16	mouseKeysMaxSpeed B16;
    CARD16	mouseKeysCurve B16;
    CARD16	accessXOptions B16;
    CARD16	accessXTimeout B16;
    CARD16	accessXTimeoutOptionsMask B16;
    CARD16	accessXTimeoutOptionsValues B16;
    CARD16	pad2 B16;
    CARD32	accessXTimeoutMask B32;
    CARD32	accessXTimeoutValues B32;
    CARD32	enabledControls B32;
    BYTE	perKeyRepeat[XkbPerKeyBitArraySize];
} xkbGetControlsReply;
#define	sz_xkbGetControlsReply	92

typedef struct _xkbSetControls {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBSetControls */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD8	affectInternalRealMods;
    CARD8	internalRealMods;
    CARD8	affectIgnoreLockRealMods;
    CARD8	ignoreLockRealMods;
    CARD16	affectInternalVirtualMods B16;
    CARD16	internalVirtualMods B16;
    CARD16	affectIgnoreLockVirtualMods B16;
    CARD16	ignoreLockVirtualMods B16;
    CARD8	mouseKeysDfltBtn;
    CARD8	groupsWrap;
    CARD16	accessXOptions B16;
    CARD16	pad1 B16;
    CARD32	affectEnabledControls B32;
    CARD32	enabledControls B32;
    CARD32	changeControls B32;
    CARD16	repeatDelay B16;
    CARD16	repeatInterval B16;
    CARD16	slowKeysDelay B16;
    CARD16	debounceDelay B16;
    CARD16	mouseKeysDelay B16;
    CARD16	mouseKeysInterval B16;
    CARD16	mouseKeysTimeToMax B16;
    CARD16	mouseKeysMaxSpeed B16;
    CARD16	mouseKeysCurve B16;
    CARD16	accessXTimeout B16;
    CARD32	accessXTimeoutMask B32;
    CARD32	accessXTimeoutValues B32;
    CARD16	accessXTimeoutOptionsMask B16;
    CARD16	accessXTimeoutOptionsValues B16;
    BYTE	perKeyRepeat[XkbPerKeyBitArraySize];
} xkbSetControlsReq;
#define	sz_xkbSetControlsReq	100

typedef	struct _xkbKTMapEntryWireDesc {
    BOOL	active;
    CARD8	mask;
    CARD8	level;
    CARD8	realMods;
    CARD16	virtualMods B16;
    CARD16	pad B16;
} xkbKTMapEntryWireDesc;
#define sz_xkbKTMapEntryWireDesc	8

typedef struct _xkbKTSetMapEntryWireDesc {
    CARD8	level;
    CARD8	realMods;
    CARD16	virtualMods B16;
} xkbKTSetMapEntryWireDesc;
#define	sz_xkbKTSetMapEntryWireDesc	4

typedef struct _xkbModsWireDesc {
    CARD8	mask;		/* GetMap only */
    CARD8	realMods;
    CARD16	virtualMods B16;
} xkbModsWireDesc;
#define	sz_xkbModsWireDesc	4

typedef struct _xkbKeyTypeWireDesc {
    CARD8	mask;
    CARD8	realMods;
    CARD16	virtualMods B16;
    CARD8	numLevels;
    CARD8	nMapEntries;
    BOOL	preserve;
    CARD8	pad;
} xkbKeyTypeWireDesc;
#define	sz_xkbKeyTypeWireDesc	8

typedef struct _xkbSymMapWireDesc {
    CARD8	ktIndex[XkbNumKbdGroups];
    CARD8	groupInfo;
    CARD8	width;
    CARD16	nSyms B16;
} xkbSymMapWireDesc;
#define	sz_xkbSymMapWireDesc	8

typedef struct _xkbVModMapWireDesc {
    KeyCode	key;
    CARD8	pad;
    CARD16	vmods B16;
} xkbVModMapWireDesc;
#define	sz_xkbVModMapWireDesc	4;

typedef struct _xkbGetMap {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetMap */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	full B16;
    CARD16	partial B16;
    CARD8	firstType;
    CARD8	nTypes;
    KeyCode	firstKeySym;
    CARD8	nKeySyms;
    KeyCode	firstKeyAction;
    CARD8	nKeyActions;
    KeyCode	firstKeyBehavior;
    CARD8	nKeyBehaviors;
    CARD16	virtualMods B16;
    KeyCode	firstKeyExplicit;
    CARD8	nKeyExplicit;
    KeyCode	firstModMapKey;
    CARD8	nModMapKeys;
    KeyCode	firstVModMapKey;
    CARD8	nVModMapKeys;
    CARD16	pad1 B16;
} xkbGetMapReq;
#define	sz_xkbGetMapReq	28

typedef struct _xkbGetMapReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD16	pad1 B16;
    KeyCode	minKeyCode;
    KeyCode	maxKeyCode;
    CARD16	present B16;
    CARD8	firstType;
    CARD8	nTypes;
    CARD8	totalTypes;
    KeyCode	firstKeySym;
    CARD16	totalSyms B16;
    CARD8	nKeySyms;
    KeyCode	firstKeyAction;
    CARD16	totalActions B16;
    CARD8	nKeyActions;
    KeyCode	firstKeyBehavior;
    CARD8	nKeyBehaviors;
    CARD8	totalKeyBehaviors;
    KeyCode	firstKeyExplicit;
    CARD8	nKeyExplicit;
    CARD8	totalKeyExplicit;
    KeyCode	firstModMapKey;
    CARD8	nModMapKeys;
    CARD8	totalModMapKeys;
    KeyCode	firstVModMapKey;
    CARD8	nVModMapKeys;
    CARD8	totalVModMapKeys;
    CARD8	pad2;
    CARD16	virtualMods B16;
} xkbGetMapReply;
#define	sz_xkbGetMapReply		40

typedef struct _xkbSetMap {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBSetMap */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	present B16;
    CARD16	resize B16;
    KeyCode	minKeyCode;
    KeyCode	maxKeyCode;
    CARD8	firstType;
    CARD8	nTypes;
    KeyCode	firstKeySym;
    CARD8	nKeySyms;
    CARD16	totalSyms B16;
    KeyCode	firstKeyAction;
    CARD8	nKeyActions;
    CARD16	totalActions B16;
    KeyCode	firstKeyBehavior;
    CARD8	nKeyBehaviors;
    CARD8	totalKeyBehaviors;
    KeyCode	firstKeyExplicit;
    CARD8	nKeyExplicit;
    CARD8	totalKeyExplicit;
    KeyCode	firstModMapKey;
    CARD8	nModMapKeys;
    CARD8	totalModMapKeys;
    KeyCode	firstVModMapKey;
    CARD8	nVModMapKeys;
    CARD8	totalVModMapKeys;
    CARD16	virtualMods B16;
} xkbSetMapReq;
#define	sz_xkbSetMapReq	36

typedef struct _xkbBehaviorWireDesc {
	CARD8	key;
	CARD8	type;
	CARD8	data;
	CARD8	pad;
} xkbBehaviorWireDesc;
#define	sz_xkbBehaviorWireDesc	4

typedef	struct _xkbActionWireDesc {
    CARD8	type;
    CARD8	data[7];
} xkbActionWireDesc;
#define	sz_xkbActionWireDesc	8

typedef struct _xkbSymInterpretWireDesc {
    CARD32		sym B32;
    CARD8		mods;
    CARD8		match;
    CARD8		virtualMod;
    CARD8		flags;
    xkbActionWireDesc	act;
} xkbSymInterpretWireDesc;
#define	sz_xkbSymInterpretWireDesc	16

typedef struct _xkbGetCompatMap {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetCompatMap */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD8	groups;
    BOOL	getAllSI;
    CARD16	firstSI B16;
    CARD16	nSI B16;
} xkbGetCompatMapReq;
#define	sz_xkbGetCompatMapReq	12

typedef struct _xkbGetCompatMapReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD8	groups;
    CARD8	pad1;
    CARD16	firstSI B16;
    CARD16	nSI B16;
    CARD16	nTotalSI B16;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
} xkbGetCompatMapReply;
#define	sz_xkbGetCompatMapReply		32

typedef struct _xkbSetCompatMap {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBSetCompatMap */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD8	pad1;
    BOOL	recomputeActions;
    BOOL	truncateSI;
    CARD8	groups;
    CARD16	firstSI B16;
    CARD16	nSI B16;
    CARD16	pad2 B16;
} xkbSetCompatMapReq;
#define	sz_xkbSetCompatMapReq	16

typedef struct _xkbGetIndicatorState {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetIndicatorState */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad B16;
} xkbGetIndicatorStateReq;
#define	sz_xkbGetIndicatorStateReq	8

typedef struct _xkbGetIndicatorStateReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD32	state B32;
    CARD32	pad1 B32;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
} xkbGetIndicatorStateReply;
#define	sz_xkbGetIndicatorStateReply	32

typedef struct _xkbGetIndicatorMap {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetIndicatorMap */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad B16;
    CARD32	which B32;
} xkbGetIndicatorMapReq;
#define	sz_xkbGetIndicatorMapReq	12

typedef struct _xkbGetIndicatorMapReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD32	which B32;
    CARD32	realIndicators B32;
    CARD8	nIndicators;
    CARD8	pad1;
    CARD16	pad2 B16;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
} xkbGetIndicatorMapReply;
#define	sz_xkbGetIndicatorMapReply	32

typedef struct _xkbIndicatorMapWireDesc {
    CARD8	flags;
    CARD8	whichGroups;
    CARD8	groups;
    CARD8	whichMods;
    CARD8	mods;
    CARD8	realMods;
    CARD16	virtualMods B16;
    CARD32	ctrls B32;
} xkbIndicatorMapWireDesc;
#define	sz_xkbIndicatorMapWireDesc	12

typedef struct _xkbSetIndicatorMap {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBSetIndicatorMap */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad1 B16;
    CARD32	which B32;
} xkbSetIndicatorMapReq;
#define	sz_xkbSetIndicatorMapReq	12

typedef struct _xkbGetNames {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetNames */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad B16;
    CARD32	which B32;
} xkbGetNamesReq;
#define	sz_xkbGetNamesReq		12

typedef	struct _xkbGetNamesReply {
    BYTE	type;
    BYTE	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD32	which B32;
    KeyCode	minKeyCode;
    KeyCode	maxKeyCode;
    CARD8	nTypes;
    CARD8	groupNames;
    CARD16	virtualMods B16;
    KeyCode	firstKey;
    CARD8	nKeys;
    CARD32	indicators B32;
    CARD8	nRadioGroups;
    CARD8	nKeyAliases;
    CARD16	pad2 B16;
    CARD32	pad3 B32;
} xkbGetNamesReply;
#define	sz_xkbGetNamesReply	32

typedef struct _xkbSetNames {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBSetNames */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	virtualMods B16;
    CARD32	which B32;
    CARD8	firstType;
    CARD8	nTypes;
    CARD8	firstKTLevel;
    CARD8	nKTLevels;
    CARD32	indicators B32;
    CARD8	groupNames;
    CARD8	nRadioGroups;
    KeyCode	firstKey;
    CARD8	nKeys;
    CARD8	nKeyAliases;
    CARD8	pad1;
    CARD16	pad2;
    CARD32	resize B32;
} xkbSetNamesReq;
#define	sz_xkbSetNamesReq	32

typedef struct _xkbPointWireDesc {
    INT16	x B16;
    INT16	y B16;
} xkbPointWireDesc;
#define	sz_xkbPointWireDesc	4

typedef struct _xkbOutlineWireDesc {
    CARD8	nPoints;
    CARD8	cornerRadius;
    CARD16	pad B16;
} xkbOutlineWireDesc;
#define	sz_xkbOutlineWireDesc	4

typedef struct _xkbShapeWireDesc {
    Atom	name B32;
    CARD8	nOutlines;
    CARD8	primaryNdx;
    CARD8	approxNdx;
    CARD8	pad;
} xkbShapeWireDesc;
#define	sz_xkbShapeWireDesc	8

typedef struct _xkbSectionWireDesc {
    Atom	name B32;
    INT16	top B16;
    INT16	left B16;
    CARD16	width B16;
    CARD16	height B16;
    INT16	angle B16;
    CARD8	priority;
    CARD8	nRows;
    CARD8	nDoodads;
    CARD8	nOverlays;
    CARD16	pad B16;
} xkbSectionWireDesc;
#define	sz_xkbSectionWireDesc	20

typedef struct _xkbRowWireDesc {
    INT16	top B16;
    INT16	left B16;
    CARD8	nKeys;
    BOOL	vertical;
    CARD16	pad B16;
} xkbRowWireDesc;
#define	sz_xkbRowWireDesc	8

typedef struct _xkbKeyWireDesc {
    CARD8	name[XkbKeyNameLength];
    INT16	gap B16;
    CARD8	shapeNdx;
    CARD8	colorNdx;
} xkbKeyWireDesc;
#define	sz_xkbKeyWireDesc	8

typedef struct _xkbOverlayWireDesc {
    Atom	name B32;
    CARD8	nRows;
    CARD8	pad1;
    CARD16	pad2 B16;
} xkbOverlayWireDesc;
#define	sz_xkbOverlayWireDesc	8

typedef struct _xkbOverlayRowWireDesc {
   CARD8	rowUnder;
   CARD8	nKeys;
   CARD16	pad1 B16;
} xkbOverlayRowWireDesc;
#define	sz_xkbOverlayRowWireDesc	4

typedef struct _xkbOverlayKeyWireDesc {
   CARD8	over[XkbKeyNameLength];
   CARD8	under[XkbKeyNameLength];
} xkbOverlayKeyWireDesc;
#define	sz_xkbOverlayKeyWireDesc	8

typedef struct _xkbShapeDoodadWireDesc {
    Atom	name B32;
    CARD8	type;
    CARD8	priority;
    INT16	top B16;
    INT16	left B16;
    INT16	angle B16;
    CARD8	colorNdx;
    CARD8	shapeNdx;
    CARD16	pad1 B16;
    CARD32	pad2 B32;
} xkbShapeDoodadWireDesc;
#define	sz_xkbShapeDoodadWireDesc	20

typedef struct _xkbTextDoodadWireDesc {
    Atom	name B32;
    CARD8	type;
    CARD8	priority;
    INT16	top B16;
    INT16	left B16;
    INT16	angle B16;
    CARD16	width B16;
    CARD16	height B16;
    CARD8	colorNdx;
    CARD8	pad1;
    CARD16	pad2 B16;
} xkbTextDoodadWireDesc;
#define	sz_xkbTextDoodadWireDesc	20

typedef struct _xkbIndicatorDoodadWireDesc {
    Atom	name B32;
    CARD8	type;
    CARD8	priority;
    INT16	top B16;
    INT16	left B16;
    CARD8	shapeNdx;
    CARD8	onColorNdx;
    CARD8	offColorNdx;
    CARD8	pad1;
    CARD16	pad2 B16;
    CARD32	pad3 B32;
} xkbIndicatorDoodadWireDesc;
#define	sz_xkbIndicatorDoodadWireDesc	20

typedef struct _xkbLogoDoodadWireDesc {
    Atom	name B32;
    CARD8	type;
    CARD8	priority;
    INT16	top B16;
    INT16	left B16;
    INT16	angle B16;
    CARD8	colorNdx;
    CARD8	shapeNdx;
    CARD16	pad1 B16;
    CARD32	pad2 B32;
} xkbLogoDoodadWireDesc;
#define	sz_xkbLogoDoodadWireDesc	20

typedef struct _xkbAnyDoodadWireDesc {
    Atom	name B32;
    CARD8	type;
    CARD8	priority;
    INT16	top B16;
    INT16	left B16;
    CARD16	pad1 B16;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
} xkbAnyDoodadWireDesc;
#define	sz_xkbAnyDoodadWireDesc	20

typedef union _xkbDoodadWireDesc {
    xkbAnyDoodadWireDesc	any;
    xkbShapeDoodadWireDesc	shape;
    xkbTextDoodadWireDesc	text;
    xkbIndicatorDoodadWireDesc	indicator;
    xkbLogoDoodadWireDesc	logo;
} xkbDoodadWireDesc;
#define	sz_xkbDoodadWireDesc	20

typedef struct _xkbGetGeometry {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetGeometry */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad B16;
    Atom	name B32;
} xkbGetGeometryReq;
#define	sz_xkbGetGeometryReq	12

typedef struct _xkbGetGeometryReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    Atom	name B32;
    BOOL	found;
    CARD8	pad;
    CARD16	widthMM B16;
    CARD16	heightMM B16;
    CARD16	nProperties B16;
    CARD16	nColors B16;
    CARD16	nShapes B16;
    CARD16	nSections B16;
    CARD16	nDoodads B16;
    CARD8	baseColorNdx;
    CARD8	labelColorNdx;
    CARD16	pad1 B16;
} xkbGetGeometryReply;
#define	sz_xkbGetGeometryReply	32

typedef struct _xkbSetGeometry {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBSetGeometry */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD8	nShapes;
    CARD8	nSections;
    Atom	name B32;
    CARD16	widthMM B16;
    CARD16	heightMM B16;
    CARD16	nProperties B16;
    CARD16	nColors B16;
    CARD16	nDoodads B16;
    CARD8	baseColorNdx;
    CARD8	labelColorNdx;
} xkbSetGeometryReq;
#define	sz_xkbSetGeometryReq	24

typedef struct _xkbPerClientFlags {
    CARD8	reqType;
    CARD8	xkbReqType;/* always X_KBPerClientFlags */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad1 B16;
    CARD32	change B32;
    CARD32	value B32;
    CARD32	ctrlsToChange B32;
    CARD32	autoCtrls B32;
    CARD32	autoCtrlValues B32;
} xkbPerClientFlagsReq;
#define	sz_xkbPerClientFlagsReq	28

typedef struct _xkbPerClientFlagsReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD32	supported B32;
    CARD32	value B32;
    CARD32	autoCtrls B32;
    CARD32	autoCtrlValues B32;
    CARD32	pad1 B32;
    CARD32	pad2 B32;
} xkbPerClientFlagsReply;
#define	sz_xkbPerClientFlagsReply	32

typedef struct _xkbListComponents {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBListComponents */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	pad B16;
} xkbListComponentsReq;
#define	sz_xkbListComponentsReq	8

typedef struct _xkbListComponentsReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD16	nKeymaps B16;
    CARD16	nKeycodes B16;
    CARD16	nTypes B16;
    CARD16	nCompatMaps B16;
    CARD16	nSymbols B16;
    CARD16	nGeometries B16;
} xkbListComponentsReply;
#define	sz_xkbListComponentsReply	32

typedef struct _xkbGetKbdByName {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetKbdByName */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD16	need B16;	/* combination of XkbGBN_* */
    CARD16	want B16;	/* combination of XkbGBN_* */
    BOOL	load;
    BOOL	newKeyboard;
} xkbGetKbdByNameReq;
#define	sz_xkbGetKbdByNameReq	12

typedef struct _xkbGetKbdByNameReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    KeyCode	minKeyCode;
    KeyCode	maxKeyCode;
    BOOL	loaded;
    BOOL	newKeyboard;
    CARD16	found B16;	/* combination of XkbGBN_* */
    CARD16	reported B16;	/* combination of XkbAllComponents */
    CARD8	compiledNameLen;
    CARD8	pad1;
    CARD16	pad2 B16;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
} xkbGetKbdByNameReply;
#define	sz_xkbGetKbdByNameReply	32

typedef struct _xkbGetButtonActions {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBGetButtonActions */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD8	firstBtn;
    CARD8	nBtns;
} xkbGetButtonActionsReq;
#define	sz_xkbGetButtonActions	8

typedef struct _xkbGetButtonActionsReply {
    CARD8	type;		/* always X_Reply */
    CARD8	deviceID;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD8	firstBtn;
    CARD8	nBtns;
    CARD16	pad1 B16;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
    CARD32	pad6 B32;
} xkbGetButtonActionsReply;
#define	sz_xkbGetButtonActionsReply	32

typedef struct _xkbSetButtonActions {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBSetButtonActions */
    CARD16	length B16;
    CARD16	deviceSpec B16;
    CARD8	firstBtn;
    CARD8	nBtns;
} xkbSetButtonActionsReq;
#define	sz_xkbSetButtonActions	8

#define	XkbNormalLocks	0
#define	XkbDisableLocks	1
#define	XkbLeaveLocks	2
typedef struct _xkbSetDebuggingFlags {
    CARD8	reqType;
    CARD8	xkbReqType;	/* always X_KBSetDebuggingFlags */
    CARD16	length B16;
    CARD16	mask B16;
    CARD16	flags B16;
    CARD16	msgLength B16;
    CARD8	disableLocks;
    CARD8	pad;
} xkbSetDebuggingFlagsReq;
#define	sz_xkbSetDebuggingFlagsReq	12

typedef struct _xkbSetDebuggingFlagsReply {
    BYTE	type;		/* X_Reply */
    CARD8	disableLocks;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD16	currentFlags B16;
    CARD16	pad1 B16;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
    CARD32	pad6 B32;
} xkbSetDebuggingFlagsReply;
#define	sz_xkbSetDebuggingFlagsReply	32

	/*
	 * X KEYBOARD EXTENSION EVENT STRUCTURES
	 */

typedef struct _xkbAnyEvent {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	pad1;
    CARD16	pad2 B16;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
    CARD32	pad6 B32;
    CARD32	pad7 B32;
} xkbAnyEvent;
#define	sz_xkbAnyEvent 32

typedef	struct _xkbNewKeyboardNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    KeyCode	minKeyCode;
    KeyCode	maxKeyCode;
    CARD8	oldDeviceID;
    KeyCode	oldMinKeyCode;
    KeyCode	oldMaxKeyCode;
    KeyCode	keycode;
    CARD8	eventType;
    CARD8	requestMajor;
    CARD8	requestMinor;
    CARD16	pad1 B16;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
} xkbNewKeyboardNotify;
#define	sz_xkbNewKeyboardNotify	32

typedef	struct _xkbMapNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	ptrBtnActions;
    CARD8	deviceID;
    CARD16	changed B16;
    KeyCode	minKeyCode;
    KeyCode	maxKeyCode;
    CARD8	firstType;
    CARD8	nTypes;
    KeyCode	firstKeySym;
    CARD8	nKeySyms;
    KeyCode	firstKeyAction;
    CARD8	nKeyActions;
    KeyCode	firstKeyBehavior;
    CARD8	nKeyBehaviors;
    CARD16	virtualMods B16;
    KeyCode	firstKeyExplicit;
    CARD8	nKeyExplicit;
    KeyCode	firstModMapKey;
    CARD8	nModMapKeys;
    KeyCode	firstVModMapKey;
    CARD8	nVModMapKeys;
    CARD16	pad1 B16;
} xkbMapNotify;
#define	sz_xkbMapNotify	32

typedef	struct _xkbStateNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	mods;
    CARD8	baseMods;
    CARD8	latchedMods;
    CARD8	lockedMods;
    CARD8	group;
    CARD8	baseGroup;
    CARD8	latchedGroup;
    CARD8	lockedGroup;
    CARD8	compatState;
    CARD8	grabMods;
    CARD8	compatGrabMods;
    CARD8	lookupMods;
    CARD8	compatLookupMods;
    CARD16	ptrBtnState B16;
    CARD16	changed B16;
    KeyCode	keycode;
    CARD8	eventType;
    CARD8	requestMajor;
    CARD8	requestMinor;
    CARD16	pad B16;
} xkbStateNotify;
#define	sz_xkbStateNotify	32

typedef struct _xkbControlsNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	numGroups;
    CARD16	pad2 B16;
    CARD32	changedControls B32;
    CARD32	enabledControls B32;
    CARD32	enabledControlChanges B32;
    KeyCode	keycode;
    CARD8	eventType;
    CARD8	requestMajor;
    CARD8	requestMinor;
    CARD32	pad3 B32;
} xkbControlsNotify;
#define	sz_xkbControlsNotify	32

typedef struct _xkbIndicatorNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	pad1;
    CARD16	pad2 B16;
    CARD32	stateChanged B32;
    CARD32	state B32;
    CARD32	mapChanged B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
} xkbIndicatorNotify;
#define	sz_xkbIndicatorNotify	32

typedef struct _xkbNamesNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	pad1;
    CARD16	changed B16;
    CARD8	firstType;
    CARD8	nTypes;
    CARD8	firstLevelName;
    CARD8	nLevelNames;
    CARD8	firstRadioGroup;
    CARD8	nRadioGroups;
    CARD8	nCharSets;
    CARD8	changedGroupNames;
    CARD16	changedVirtualMods B16;
    CARD16	pad3 B16;
    CARD32	changedIndicators B32;
    CARD32	pad4 B32;
} xkbNamesNotify;
#define	sz_xkbNamesNotify	32

typedef struct _xkbCompatMapNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	changedGroups;
    CARD16	firstSI B16;
    CARD16	nSI B16;
    CARD16	nTotalSI B16;
    CARD32	pad1 B32;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
} xkbCompatMapNotify;
#define sz_xkbCompatMapNotify	32

typedef struct _xkbBellNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	bellClass;
    CARD8	bellID;
    CARD8	percent;
    CARD16	pitch B16;
    CARD16	duration B16;
    Atom	name B32;
    Window	window B32;
    BOOL	eventOnly;
    CARD8	pad1;
    CARD16	pad2 B16;
    CARD32	pad3 B32;
} xkbBellNotify;
#define	sz_xkbBellNotify	32

typedef struct _xkbActionMessage {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    KeyCode	keycode;
    BOOL	press;
    BOOL	keyEventFollows;
    CARD8	message[8];
    CARD32	pad1 B32;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
} xkbActionMessage;
#define	sz_xkbActionMessage		32

typedef struct _xkbSlowKeyNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	slowKeyState;
    KeyCode	keycode;
    CARD8	pad1;
    CARD16	delay B16;
    CARD16	pad2 B16;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
    CARD32	pad5 B32;
    CARD32	pad6 B32;
} xkbSlowKeyNotify;
#define	sz_xkbSlowKeyNotify	32

typedef struct _xkbButtonActionsNotify {
    BYTE	type;
    BYTE	xkbType;
    CARD16	sequenceNumber B16;
    Time	time B32;
    CARD8	deviceID;
    CARD8	firstBtn;
    CARD8	nBtns;
    CARD8	count;
    BYTE	changed[16];
    CARD32	pad B32;
} xkbButtonActionsNotify;
#define	sz_xkbButtonActionsNotify		32

typedef struct _xkbEvent {
    union {
	xkbAnyEvent		any;
	xkbNewKeyboardNotify	new_kbd;
	xkbMapNotify		map;
	xkbStateNotify		state;
	xkbControlsNotify	ctrls;
	xkbIndicatorNotify	indicators;
	xkbNamesNotify		names;
	xkbCompatMapNotify	compat;
	xkbBellNotify		bell;
	xkbActionMessage	message;
	xkbSlowKeyNotify	slowkey;
	xkbButtonActionsNotify	btn_acts;
    } u;
} xkbEvent;
#define sz_xkbEvent	32

#undef Window
#undef Atom
#undef Time
#undef KeyCode
#undef KeySym

#endif /* _XKBPROTO_H_ */
