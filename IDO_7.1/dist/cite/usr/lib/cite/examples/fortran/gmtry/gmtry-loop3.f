	program main
	real*4 t1, t2(2), t3(2)
	integer m
	
	m = 100
	t1 = etime(t2) 
	call gmtry(m)
	t1 = etime(t3) 
	print *, "Time = ", t3(1) - t2(1)
	stop
	end
	
	subroutine gmtry(matdim)
        implicit double precision (a-h, o-z)
        parameter (nw = 100, nb = 5)
        complex wall, zcr, proj
        common /arrays/ wall(nw, nb), rmatrx(nw * nb, nw * nb), zcr(nw,
     *nb), proj(nw, nb), xmax(nb), nwall(nb)
C
        data arcl /5.d0/, pi /3.141592653589793d0/, period /3.d0/
C
        do 8 i = 1, matdim
          rmatrx(i, i) = 1.d0 / rmatrx(i, i)
          do 8 j = i + 1, matdim
            rmatrx(j, i) = rmatrx(j, i) * rmatrx(i, i)
            do 8 k = i + 1, matdim
              rmatrx(j, k) = rmatrx(j, k) - rmatrx(j, i) * rmatrx(i, k)
8       continue
	return
	end
