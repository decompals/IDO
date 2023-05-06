	subroutine ll2car (long, lat, x, y, z)

	  real long, lat
	  real x, y, z

	  real d2r
	  parameter (d2r = 0.0174532925)

	  rlong = long * d2r
	  rlat = lat * d2r
	  x = cos(rlong) * cos(rlat)
	  y = sin(rlong) * cos(rlat)
	  z = sin(rlat)
	  return
	end
