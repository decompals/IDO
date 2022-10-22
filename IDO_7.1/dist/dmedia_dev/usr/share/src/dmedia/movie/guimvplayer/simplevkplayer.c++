/*
 * File:        simplevkplayer.c++
 *
 * Usage:       simplevkplayer <moviefilename>
 *
 * Description: Very simple SGI ViewKit(tm) application for playing movies.
 *
 * Functions:   Uses mvIsMovieFile() and the MovieController.
 *
 */

#include <stdio.h>
#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <MovieController.h>

void main ( int argc, char **argv )
{
    if ( argc != 2 ) {
        fprintf( stderr, "usage: %s moviefile\n", argv[0] );
        exit( EXIT_FAILURE );
    }
    if ( !( mvIsMovieFile( argv[1] ) ) ) {
        fprintf( stderr, "%s: %s is not a moviefile.\n", argv[0], argv[1] );
        exit( EXIT_FAILURE );
    }

    VkApp		*app   = new VkApp( "simplevkplayer", &argc, argv );
    VkSimpleWindow	*win   = new VkSimpleWindow( "simplevkplayer" );

    MovieController *movieWidget = new MovieController( win->mainWindowWidget(),
                                                       "movie", argv[1], 1 );
    win->setTitle( argv[1] );

    /*
     * Make the MovieController, movieWidget,  viewable.
     */

    movieWidget->show();

    /*
     * Display the window.
     */

    win->show();

    /*
     * Run the application.
     */

    app->run();
}
