/* panel.h
 * -------
 *
 * $Revision: 1.9 $
 *
 */

#define PANEL_HEIGHT 220
#define PANEL_WIDTH 720

#define inrect(x, y, rect)	\
(				\
    ((x) > (rect).left) &&	\
    ((x) < (rect).right) &&	\
    ((y) > (rect).bottom) &&	\
    ((y) < (rect).top)		\
)

typedef struct rect {
    long left, right, bottom, top;
} RECT;

#define PUSHED_	    0
#define PUSHED	    0x00000001
#define HIGHLITED_  1
#define HIGHLITED   0x00000002

#define SHIFT_PUSHED(data) ((data << PUSHED_) & PUSHED)
#define SET_PUSHED(data, state) \
    ((state) & ~PUSHED) | (SHIFT_PUSHED(data))
#define GET_PUSHED(state) ((state & PUSHED) >> PUSHED_)

#define SHIFT_HIGHLITED(data) ((data << HIGHLITED_) & HIGHLITED)
#define SET_HIGHLITED(data, state) \
    ((state) & ~HIGHLITED) | (SHIFT_HIGHLITED(data))
#define GET_HIGHLITED(state) ((state & HIGHLITED) >> HIGHLITED_)

#define PUSHED_DEFAULT	    0
#define HIGHLITED_DEFAULT   0

#define STATUS_DEFAULT \
    (SHIFT_PUSHED(PUSHED_DEFAULT) | \
    SHIFT_HIGHLITED(HIGHLITED_DEFAULT))

typedef struct item {
    char name[40];
    RECT rect;
    void (* dsp) (struct item *item);
    void (* select) (struct item *item, long dev);
    void (* action) (void);
    int *val;
    unsigned long status;
    unsigned long *color_ramp;
} ITEM;

#define ITEM_HELP   1
#define NUM_ITEMS   22

#define NUMTESSTYPES 5
#define NUMPRIMTYPES 4

extern long PanelWid;
extern int PanelButtonDown;
extern ITEM Items[];

extern void InitPanel(void);
extern void ClosePanelWindow(void);
extern void OpenPanelWindow(void);
extern void DrawPanel(void);
extern void DoEventsPanel(long dev, short val);
extern void DoPanel(long dev, short val);
