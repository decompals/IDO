#ifndef dm_connection_h
#define dm_connection_h

#include "dmedia/dm_stream.h"
#include "dmedia/dm_params.h"


/* 
 * DMbuffer encapsulates a simple buffer in memory. A DMconnection can 
 * contain a DMbuffer*. When the input and output DMconnections to a codec 
 * contain DMbuffer*, then that codec can be used for synchronous coding or 
 * decoding of a fixed number of frames.
 */
typedef struct _DMbuffer {
	void*	_data;
	int	size;
	int	sizeOfGoodData;
} DMbuffer;

/*
 * DMstream is a lose grouping of several fields that a codec will need to access 
 * in order to be able to read to or write from an event stream
 */

typedef struct _DMstream {
	DMpoolid	poolId;		/* used by codec on output connection */
	DMsenderid	senderId;	/* used by codec on both sides */
} DMstream;

/*
 * DMwire describes a hardware connection to a digital media node such as a codec.
 */
typedef struct _DMwire {
	char*     description; /* string describing the hardware connection */
	DMstream* eventStream; /* DMstream to communicate events */
} DMwire;

typedef enum __DMconnectionType
{
	DM_CONNECTION_STREAM = 100,
	DM_CONNECTION_BUFFER = 101,
	DM_CONNECTION_WIRE = 102
} DMconnectionType;

typedef struct _DMconnection {
	DMconnectionType connectionType;
	union {
		DMstream*	stream;
		DMbuffer*	buffer;
		DMwire*		wire; 
	} value;
} DMconnection;

#endif
