/*
 * nurbs.p
 *
 * $Revision: 1.4 $
 */

#include <gl/pgl.h>
#include <gl/pdevice.h>

procedure exit(i: integer); external;

program main(input, output);
const
    NUMKNOTS =	8;
    NUMCOORDS =	3;
    ORDER =	4;
    NUMPOINTS =	(NUMKNOTS - ORDER);

var
    surfknots : array[0..NUMKNOTS-1] of double;
    trimknots : array[0..11] of double;
    ctlpoints : array[0..47] of double;
    trimpoints : array[0..27] of double;
    idmat : Matrix;
    val : Short;
    zfar : longint;
    trim_flag : boolean;

procedure init_windows;
var i : longint;
begin
    if (getgdesc(GD_BITS_NORM_DBL_RED) <= 0) then begin
	writeln(
'pnurbs: requires double buffered RGB which is unavailable on this machine');
	exit(1);
    end;
    i := winopen('pnurbs');
    wintitle('NURBS Surface');
    doublebuffer;
    RGBmode;
    gconfig;
    zbuffer( TRUE );
    glcompat(GLC_ZRANGEMAP, 0);
    zfar := getgdesc(GD_ZMAX);
    setnurbsproperty(N_ERRORCHECKING, 1.0);
end;

procedure setup_queue;
begin
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
end;

procedure init_view;
begin
    mmode(MPROJECTION);
    ortho( -4.0, 4.0, -4.0, 4.0, -4.0, 4.0 ); 

    mmode(MVIEWING);
    loadmatrix(idmat);
end;

procedure set_scene;
begin
    lmbind(MATERIAL, 0);
    RGBcolor(150, 150, 150);
    lmbind(MATERIAL, 1); 
    /* make a nice background */
    czclear(16#00969696, zfar);

    rotate( 100, 'y' );
    rotate( 100, 'z' ); 
end;

procedure draw_trim_surface;
begin
    bgnsurface;
	nurbssurface( 
	    NUMKNOTS, surfknots, 
	    NUMKNOTS, surfknots,
	    8*NUMCOORDS, 8 * NUMPOINTS * NUMCOORDS, 
	    ctlpoints, 
	    ORDER, ORDER, 
	    N_V3D
	    );
	if (trim_flag = TRUE) then begin
	    bgntrim;
            nurbscurve( 
		12,
		trimknots,
		8*NUMCOORDS ,
		trimpoints,
		3,
	        N_P2DR
		);
	    endtrim;
	end;
    endsurface;
    swapbuffers;
end;

procedure make_lights;
var
    null : array[0..0] of real;
    lightarr : array[0..18] of real;
begin
    null[0] := LMNULL;

    lightarr[0] := EMISSION; 
    lightarr[1] := 0.0; 
    lightarr[2] := 0.0; 
    lightarr[3] := 0.0;
    lightarr[4] := AMBIENT;
    lightarr[5] := 0.1;
    lightarr[6] := 0.1;
    lightarr[7] := 0.1;
    lightarr[8] := DIFFUSE;
    lightarr[9] := 0.6;
    lightarr[10] := 0.3;
    lightarr[11] := 0.3;
    lightarr[12] := SPECULAR;
    lightarr[13] := 0.0;
    lightarr[14] := 0.6;
    lightarr[15] := 0.0;
    lightarr[16] := SHININESS;
    lightarr[17] := 2.0;
    lightarr[18] := LMNULL;

    lmdef(DEFLMODEL, 1, 1, null);
    lmdef(DEFLIGHT, 1, 1, null);
    lmdef(DEFMATERIAL, 1, 19, lightarr);

    lmbind(LIGHT0, 1);
    lmbind(LMODEL, 1);
end;

procedure init_arrays;
begin
    surfknots[0] := -1.0; surfknots[1] := -1.0; 
    surfknots[2] := -1.0; surfknots[3] := -1.0;
    surfknots[4] := 1.0; surfknots[5] := 1.0; 
    surfknots[6] := 1.0; surfknots[7] := 1.0;

    ctlpoints[0]  := -2.5; ctlpoints[1]  := -3.7; ctlpoints[2]  := 1.0;
    ctlpoints[3]  := -1.5; ctlpoints[4]  := -3.7; ctlpoints[5]  := 3.0;
    ctlpoints[6]  := 1.5;  ctlpoints[7]  := -3.7; ctlpoints[8]  := -2.5;
    ctlpoints[9]  := 2.5;  ctlpoints[10] := -3.7; ctlpoints[11] := -0.75;

    ctlpoints[12]  := -2.5; ctlpoints[13]  := -2.0; ctlpoints[14]  := 3.0;
    ctlpoints[15]  := -1.5; ctlpoints[16]  := -2.0; ctlpoints[17]  := 4.0;
    ctlpoints[18]  := 1.5;  ctlpoints[19]  := -2.0; ctlpoints[20]  := -3.0;
    ctlpoints[21]  := 2.5;  ctlpoints[22] := -2.0; ctlpoints[23] := 0.0;

    ctlpoints[24]  := -2.5; ctlpoints[25]  := 2.0; ctlpoints[26]  := 1.0;
    ctlpoints[27]  := -1.5; ctlpoints[28]  := 2.0;  ctlpoints[29]  := 0.0;
    ctlpoints[30]  := 1.5;  ctlpoints[31]  := 2.0;  ctlpoints[32]  := -1.0;
    ctlpoints[33]  := 2.5;  ctlpoints[34] := 2.0;  ctlpoints[35] := 2.0;

    ctlpoints[36]  := -2.5; ctlpoints[37]  := 2.7;  ctlpoints[38]  := 1.25;
    ctlpoints[39]  := -1.5; ctlpoints[40]  := 2.7;  ctlpoints[41]  := 0.1;
    ctlpoints[42]  := 1.5;  ctlpoints[43]  := 2.7;  ctlpoints[44]  := -0.6;
    ctlpoints[45]  := 2.5;  ctlpoints[46] := 2.7;  ctlpoints[47] := 0.2;

    trimknots[0] := 0.0; trimknots[1] := 0.0; trimknots[2] := 0.0; 
    trimknots[3] := 1.0; trimknots[4] := 1.0; trimknots[5] := 2.0; 
    trimknots[6] := 2.0; trimknots[7] := 3.0; trimknots[8] := 3.0; 
    trimknots[9] := 4.0; trimknots[10] := 4.0; trimknots[11] := 4.0;

    trimpoints[0]  := 1.0; trimpoints[1]  := 0.0; trimpoints[2]  := 1.0;
    trimpoints[3]  := 1.0; trimpoints[4]  := 1.0; trimpoints[5]  := 1.0;
    trimpoints[6]  := 0.0;  trimpoints[7]  := 2.0; trimpoints[8]  := 2.0;
    trimpoints[9]  := -1.0;  trimpoints[10] := 1.0; trimpoints[11] := 1.0;
    trimpoints[12]  := -1.0; trimpoints[13]  :=  0.0; trimpoints[14]  := 1.0;
    trimpoints[15]  := -1.0; trimpoints[16]  := -1.0; trimpoints[17]  := 1.0;
    trimpoints[18]  := 0.0;  trimpoints[19]  := -2.0; trimpoints[20]  := 2.0;
    trimpoints[21]  := 1.0;  trimpoints[22] := -1.0; trimpoints[23] := 1.0;
    trimpoints[24]  := 1.0; trimpoints[25]  := 0.0; trimpoints[26]  := 1.0;

    idmat[0,0] := 1.0; idmat[0,1] := 0.0; idmat[0,2] := 0.0; idmat[0,3] := 0.0;
    idmat[1,0] := 0.0; idmat[1,1] := 1.0; idmat[1,2] := 0.0; idmat[1,3] := 0.0;
    idmat[2,0] := 0.0; idmat[2,1] := 0.0; idmat[2,2] := 1.0; idmat[2,3] := 0.0;
    idmat[3,0] := 0.0; idmat[3,1] := 0.0; idmat[3,2] := 0.0; idmat[3,3] := 1.0;
end;

begin
    trim_flag := FALSE;
    init_windows;
    setup_queue;
    init_arrays;
    init_view;
    make_lights;
    set_scene;
    draw_trim_surface;

    while (TRUE) do begin
	while (qtest <> 0) do begin
	    case qread(val) of
		ESCKEY :
		    begin
		    gexit; 
		    exit(0);
		    end;
		REDRAW :
		    begin
		    reshapeviewport;
		    set_scene;
	    	    draw_trim_surface;
		    end;
		LEFTMOUSE :
		    begin
		    if (val = 0) then begin
			trim_flag := not(trim_flag);
		    end;
		    end;
		otherwise:;
	    end;
	end;
	set_scene;
	draw_trim_surface;
    end;
end.
