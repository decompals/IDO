/* **************************************************************
 *    Original contributors by Terry Weissman, Bruce Karsh, Doug Cook,
 *	Amit Shoham, Marc Callow 
 *    ViewKit/Motif version by Gints Klimanis
 *	Presenter innards by Candace Obert 
 *				1991-4
 * ************************************************************** */

#ifndef CORONA_H
#define CORONA_H

#include <Vk/VkComponent.h>
#include <Vk/VkPeriodic.h>
#include <Vk/VkRunOnce.h>
#include <Vk/VkTickMarks.h>
#include <Vk/VkWindow.h>
#include "apaneldefs.h"

#define DEFAULT_PRESENTER_POLL_INTERVAL_VALUE    0.25
#define DEFAULT_PRESENTER_POLL_INTERVAL		"0.25"

class PresenterWindow: public VkSimpleWindow 
{
  public:

    PresenterWindow( const char	*windowName, 
		    const char	*windowIconName,
		    int		graphicsFD);
    ~PresenterWindow();
    const char* className();  // Identify this class
    char    *_windowName;

    void    update(VkComponent *component);

    int		    intervalToMilliseconds(float intervalInSeconds);
    int		    _pollInterval;	// in milliSeconds 
    void	    pollState();
    VkPeriodic	    *_pollTimer;

    void gangLevelSliders(Boolean);
    void setLevelSliderTickMarkType(char type);
    void setLevelSliderValue(int channel, int value);
    void setMuteToggle(Boolean value);
    void setToneSliderValue(int value);

    int getLevelSliderValue(int channel);
    Boolean getMuteToggle();
    int getToneSliderValue();

// graphics 

protected:
    void stateChanged(IconState state);
    void handleWmDeleteMessage();
    void handleWmQuitMessage();

    void levelSliderMoved( Widget, XtPointer );
    void toneSliderMoved( Widget, XtPointer );

private:
    static void levelSliderCB( Widget, XtPointer, XtPointer );
    static void toneSliderCB( Widget, XtPointer, XtPointer );

    Boolean	_gangState;		// True = ganged
    int		_gangValue[2];
    int		_level[2];

    int		_muteStatus;
    int		_tone;

    int		_graphicsFD;
    Boolean	_sliderTickMarkType;

    Widget	    _mainSpringBox;

    Widget	    _speakerLabel;
    Widget	    _blockLabelAndSlider;
    Widget	    _springBox;

    Widget		_levelBlock;
    Widget		_levelSlider[2];
    Widget		_levelSliderLabel[2];
    VkTickMarks		*_levelSliderTickMarks;
    Widget		_muteToggle;

    Widget		_toneBlock;
    Widget		_toneSliderLabel;
    Widget		_toneSliderBox;
    VkTickMarks		    *_toneSliderTickMarks;
    Widget		    _toneSlider;

    static void enableMuteCB( Widget, XtPointer, XtPointer );
};

#endif

