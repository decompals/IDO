/*
 *
 * File:        guimvplayer.c++
 *
 * Usage:       guimvplayer <moviefile>
 *
 * Description: A simple C++ sample IRIS IM program that shows how to use the
 *              Movie Controller class library.
 *
 * Functions:   Uses mvIsMovieFile() and the MovieController.
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/Form.h>

#include <MovieController.h>

/*****************************************************************************
 *
 * main()
 *
 ****************************************************************************/

main( int argc, char **argv )
{
    Widget          topLevel;
    Widget 	    mainWindow;
    Widget          menuBar;
    Widget	    pullDownMenu;
    Widget	    cascadeButton;
    Widget 	    pushButton[3];
    Widget          workArea;
    Arg             args[5];
    XtAppContext    appContext;
    MovieController *movieWidget;
    void            FileExit();

    /*
     * Check arguments.
     */

    if ( argc != 2) {
	fprintf( stderr, "usage: %s moviefile\n", argv[0] );
	exit( EXIT_FAILURE );
    }

    if ( !( mvIsMovieFile( argv[1] ) ) ) {
	fprintf( stderr, "%s: %s is not a  moviefile.\n", argv[0], argv[1] );
	exit( EXIT_FAILURE );
    }

    /*
     * initialize Xt intrinics.
     */

    topLevel = XtAppInitialize( &appContext, "Guimvplayer", NULL,
	 		       0, &argc, argv, NULL, NULL, 0 );

    /*
     * set up a simple IRIS IM interface with a small menu.
     */

    mainWindow = XmCreateMainWindow( topLevel, "main", NULL, 0 );
    XtManageChild( mainWindow );
    
    menuBar = XmCreateMenuBar( mainWindow, "menuBar", NULL, 0 );
    XtManageChild ( menuBar );

    pullDownMenu = XmCreatePulldownMenu( menuBar, "pullDownMenu", NULL, 0 );
    XtSetArg( args[0], XmNsubMenuId, pullDownMenu );

    cascadeButton = XmCreateCascadeButton( menuBar, "File", args, 1 );
    XtManageChild( cascadeButton );

    pushButton[0] = XmCreatePushButtonGadget( pullDownMenu, "Exit", NULL, 0 );
    XtAddCallback( pushButton[0], XmNactivateCallback,
                  (XtCallbackProc)FileExit, NULL );
    XtManageChildren( pushButton ,1 );

    workArea = XmCreateForm( mainWindow, "workArea", NULL, 0 );
    XtManageChild( workArea ); 

    XmMainWindowSetAreas( mainWindow, menuBar, NULL, NULL, NULL, workArea );

    /*
     * This is the interesting part - make a new movie instance and
     * manage it.
     * Parameters:
     *      workArea is the parent widget.
     *      "movie" is what we want to call the root widget of the movie.
     *      argv[1] has the name of the movie file.
     *      The last parameter is the size for the movie (zoom).
     */

    movieWidget = new MovieController( workArea, "movie" , argv[1], 1 );
    
    movieWidget->show();

    /*
     * realize all widgets and start event loop
     */

    XtRealizeWidget( topLevel );
    XtAppMainLoop( appContext );
}

/*
 *
 * Callback to exit program.
 *
 */

void FileExit()
{
    exit( EXIT_SUCCESS );
}
