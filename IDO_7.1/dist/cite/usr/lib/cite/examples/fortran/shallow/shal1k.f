      program shallow
C       ***
C       *** benchmark weather prediction program for comparing the performance
C       *** of current supercomputers. the model is based on the paper, "the
C       *** dynamics of finite-difference models of the shallow-water
C       *** equations," by r. sadourny, j. atm. sci., vol.32, no.4, april 1975
C       *** code by paul n. swarztrauber, national center for atmospheric
C       *** research, boulder, colorado   october 1984.
C       ***
	common /arrays/ u,v,p,unew,pnew,vnew,psi,pold,uold,vold,cu,cv,z,h
	common /scalars/ a, dt, tdt, dx, dy, alpha, pi, tpi, di, dj, fsd
     *x, fsdy, tdtsdx, tdtsdy
        real u(1026, 1024), v(1026, 1024)
        real p(1026, 1024), unew(1026, 1024)
        real pnew(1026, 1024), vnew(1026, 1024)
        real psi(1026, 1024), pold(1026, 1024)
        real uold(1026, 1024), vold(1026, 1024)
        real cu(1026, 1024), cv(1026, 1024)
        real z(1026, 1024), h(1026, 1024)
        real  tdtsdx, tdtsdy
        real a, dt, tdt, dx, dy, alpha, pi, tpi, di, dj, fsd, x, fsdy

        a = 1.e6
        dt = 90.
        tdt = 90.
        time = 0.
        dx = 1.e-5
        dy = 1.e-5
        alpha = .001
        iter = 1
c       pi = 4. * atan(1.)
        pi = 3.14159
        tpi = pi + pi
        di = tpi / float(1024 - 2)
        dj = tpi / float(1024 - 1)
C       *** initial values of the stream function
        do j = 1, 1024
          do i = 1, 1024
c           psi(i, j) = a * sin((float(i) - .5) * di) * sin((float(j) - 
c    *.5) * dj)
            psi(i, j) = a * ((float(i) - .5) * di) * ((float(j) - 
     *.5) * dj)/1025
          enddo
        enddo
        t1 = etime(t2)
C       *** initialize velocities
        do j = 1, 1024 - 1
          do i = 1, 1024 - 1
            u(i + 1, j) = -(psi(i + 1, j + 1) - psi(i + 1, j)) * dy
          enddo
        enddo
        do j = 1, 1024 - 1
          do i = 1, 1024 - 1
            v(i, j + 1) = (psi(i + 1, j + 1) - psi(i, j + 1)) * dx
          enddo
        enddo

	call compute()

	print *,'u(5,5)='
	print '(4e15.4)', u(5,5)
	print *,'v(5,5)='
	print '(4e15.4)', v(5,5)
	print *,'p(5,5)='
	print '(4e15.4)', p(5,5)

	stop
	end

	subroutine compute()
	common /arrays/ u,v,p,unew,pnew,vnew,psi,pold,uold,vold,cu,cv,z,h
	common /scalars/ a, dt, tdt, dx, dy, alpha, pi, tpi, di, dj, fsd
     *x, fsdy, tdtsdx, tdtsdy
        real u(1026, 1024), v(1026, 1024)
        real p(1026, 1024), unew(1026, 1024)
        real pnew(1026, 1024), vnew(1026, 1024)
        real psi(1026, 1024), pold(1026, 1024)
        real uold(1026, 1024), vold(1026, 1024)
        real cu(1026, 1024), cv(1026, 1024)
        real z(1026, 1024), h(1026, 1024)
        real  tdtsdx, tdtsdy
        real a, dt, tdt, dx, dy, alpha, pi, tpi, di, dj, fsd, x, fsdy

C	call clear_timer(1)
C	call start_timer(1)

C       *** periodic continuation
        do j = 1, 1024 - 1
          u(1, j) = u(1024, j)
        enddo
        do j = 1, 1024 - 1
          v(1024, j + 1) = v(1, j + 1)
        enddo
        do i = 1, 1024 - 1
          u(i + 1, 1024) = u(i + 1, 1)
        enddo
        do i = 1, 1024 - 1
          v(i, 1) = v(i, 1024)
        enddo
        u(1, 1024) = u(1024, 1)
        v(1024, 1) = v(1, 1024)
        do j = 1, 1024
          do i = 1, 1024
            uold(i, j) = u(i, j)
            vold(i, j) = v(i, j)
            pold(i, j) = 50000.
            p(i, j) = 50000.
          enddo
        enddo

C       *** compute capital u, capital v, z, and h
        fsdx = 4. / dx
        fsdy = 4. / dy
        do j = 1, 1024 - 1
          do i = 1, 1024 - 1
            cu(i + 1, j) = .5 * (p(i + 1, j) + p(i, j)) * u(i + 1, j)
            h(i, j) = p(i, j) + .25 * (u(i + 1, j) * u(i + 1, j) + u(i, 
     *j) * u(i, j) + v(i, j + 1) * v(i, j + 1) + v(i, j) * v(i, j))
          enddo
        enddo
        do j = 1, 1024 - 1
          do i = 1, 1024 - 1
            cv(i, j + 1) = .5 * (p(i, j + 1) + p(i, j)) * v(i, j + 1)
            z(i + 1, j + 1) = (fsdx * (v(i + 1, j + 1) - v(i, j + 1)) - 
     *fsdy * (u(i + 1, j + 1) - u(i + 1, j))) / (p(i, j) + p(i + 1, j) +
     * p(i + 1, j + 1) + p(i, j + 1))
          enddo
        enddo

C	call stop_timer(1)
C	call print_timer(1,'shallow 1K u,v,z,h')

C	call clear_timer(2)
C	call start_timer(2)

C       *** periodic continuation
        do j = 1, 1024 - 1
          cv(1024, j + 1) = cv(1, j + 1)
          z(1, j + 1) = z(1024, j + 1)
        enddo
        do j = 1, 1024 - 1
          cu(1, j) = cu(1024, j)
          h(1024, j) = h(1, j)
        enddo
        do i = 1, 1024 - 1
          cv(i, 1) = cv(i, 1024)
          z(i + 1, 1) = z(i + 1, 1024)
        enddo
        do i = 1, 1024 - 1
          cv(i, 1) = cv(i, 1024)
          z(i + 1, 1) = z(i + 1, 1024)
        enddo
        cu(1, 1024) = cu(1024, 1)
        cv(1024, 1) = cv(1, 1024)
        z(1, 1) = z(1024, 1024)
        h(1024, 1024) = h(1, 1)
C       *** compute new values u, v, and p
        tdts8 = tdt / 8.
        tdtsdx = tdt / dx
        tdtsdy = tdt / dy
        do j = 1, 1024 - 1
          do i = 1, 1024 - 1
            unew(i + 1, j) = uold(i + 1, j) + tdts8 * (z(i + 1, j + 1) +
     * z(i + 1, j)) * (cv(i + 1, j + 1) + cv(i, j + 1) + cv(i, j) + cv(i
     * + 1, j)) - tdtsdx * (h(i + 1, j) - h(i, j))
            pnew(i, j) = pold(i, j) - tdtsdx * (cu(i + 1, j) - cu(i, j))
     * - tdtsdy * (cv(i, j + 1) - cv(i, j))
          enddo
        enddo
        do j = 1, 1024 - 1
          do i = 1, 1024 - 1
            vnew(i, j + 1) = vold(i, j + 1) - tdts8 * (z(i + 1, j + 1) +
     * z(i, j + 1)) * (cu(i + 1, j + 1) + cu(i, j + 1) + cu(i, j) + cu(i
     * + 1, j)) - tdtsdy * (h(i, j + 1) - h(i, j))
          enddo
        enddo

C	call stop_timer(2)
C	call print_timer(2,'shallow 1K u,v,p')

C       *** periodic continuation
        do j = 1, 1024 - 1
          unew(1, j) = unew(1024, j)
          pnew(1024, j) = pnew(1, j)
        enddo
        do j = 1, 1024 - 1
          vnew(1024, j + 1) = vnew(1, j + 1)
        enddo
        do i = 1, 1024 - 1
          unew(i + 1, 1024) = unew(i + 1, 1)
          pnew(i, 1024) = pnew(i, 1)
        enddo
        do i = 1, 1024 - 1
          vnew(i, 1) = vnew(i, 1024)
        enddo
        unew(1, 1024) = unew(1024, 1)
        vnew(1024, 1) = vnew(1, 1024)
        pnew(1024, 1024) = pnew(1, 1)
        tdt = tdt + tdt
        do j = 1, 1024
          do i = 1, 1024
            uold(i, j) = u(i, j)
            vold(i, j) = v(i, j)
            pold(i, j) = p(i, j)
            u(i, j) = unew(i, j)
            v(i, j) = vnew(i, j)
            p(i, j) = pnew(i, j)
          enddo
        enddo

        return
      end
