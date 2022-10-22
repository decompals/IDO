
        program btrix

        parameter (jd = 50, kd = 50, ld = 50, md = 50)
        implicit double precision (a-h, o-z)
        common /arrays/ s(jd, kd, ld, 5), a(5, 5, md, md), b(5, 5, md, m
     *d), c(5, 5, md, md)

c	initialization

        do i=1,5
          do j=1,ld
            do k=1,kd
              do l=1,jd
                s(l,k,j,i)=l+k+j*0.1
              end do
            end do
          end do
        end do

        do i=1,md
          do j=1,md
            do k=1,5
              do l=1,5
                a(l,k,j,i)=l-k-j*0.1
                b(l,k,j,i)=l-k+j*0.1
                c(l,k,j,i)=l+k-j*0.1
              end do
            end do
          end do
        end do

C	call clear_timer(1)
C	call start_timer(1)
	do i = 1, 2500
	    call btrix1(12, 14, 1, 50, 5, 8)
        enddo
C	call stop_timer(1)
C        call print_timer(1,'btrix1 2500 iterations')
        print *,'b(1,1,1,1)= '
        print '(g15.8)',b(1,1,1,1)

C	call clear_timer(2)
C	call start_timer(2)
	do i = 1, 5000
	    call btrix2(12, 14, 1, 50, 5, 8)
        enddo
C	call stop_timer(2)
C        call print_timer(2,'btrix2 5000 iterations')
        print *,'s(1,1,1,1)= '
        print '(g15.8)',s(1,1,1,1)

        print *,'s(1,1,1,1)= '
        print '(g15.8)',s(1,1,1,1)

	do i = 1, 500
	    call btrix5(12, 14, 1, 50, 5)
        enddo
	stop
	end

	subroutine btrix1(js, je, ls, le, k, j)
        parameter (jd = 50, kd = 50, ld = 50, md = 50)
        implicit double precision (a-h, o-z)

        common /arrays/ s(jd, kd, ld, 5), a(5, 5, md, md), b(5, 5, md, m
     *d), c(5, 5, md, md)
        dimension u12(md), u13(md), u14(md), u15(md), u23(md), u24(md),
     *u25(md), u34(md), u35(md), u45(md)
C
        double precision l11(md), l21(md), l31(md), l41(md), l51(md), l2
     *2(md), l32(md), l42(md), l52(md), l33(md), l43(md), l53(md), l44(m
     *d), l54(md), l55(md)
C
          do 3 m = 1, 5
            do 3 n = 1, 5
              do 3 l = ls, le
                b(m, n, j, l) = b(m, n, j, l) - a(m, 1, j, l) * b(1, n,
     *j - 1, l) - a(m, 2, j, l) * b(2, n, j - 1, l) - a(m, 3, j, l) * b(
     *3, n, j - 1, l) - a(m, 4, j, l) * b(4, n, j - 1, l) - a(m, 5, j, l
     *) * b(5, n, j - 1, l)
3         continue
	return
	end

	subroutine btrix2(js, je, ls, le, k, j)
        implicit double precision (a-h, o-z)
        parameter (jd = 50, kd = 50, ld = 50, md = 50)

        common /arrays/ s(jd, kd, ld, 5), a(5, 5, md, md), b(5, 5, md, m
     *d), c(5, 5, md, md)
        dimension u12(md), u13(md), u14(md), u15(md), u23(md), u24(md),
     *u25(md), u34(md), u35(md), u45(md)
C
        double precision l11(md), l21(md), l31(md), l41(md), l51(md), l2
     *2(md), l32(md), l42(md), l52(md), l33(md), l43(md), l53(md), l44(m
     *d), l54(md), l55(md)
C
         do 33 m = 1, 5
            do 33 l = ls, le
              s(j, k, l, m) = s(j, k, l, m) - a(m, 1, j, l) * s(j - 1, k
     *, l, 1) - a(m, 2, j, l) * s(j - 1, k, l, 2) - a(m, 3, j, l) * s(j
     *- 1, k, l, 3) - a(m, 4, j, l) * s(j - 1, k, l, 4) - a(m, 5, j, l)
     ** s(j - 1, k, l, 5)
33        continue

	return
	end

	subroutine btrix5(js, je, ls, le, k)
        implicit double precision (a-h, o-z)
        parameter (jd = 50, kd = 50, ld = 50, md = 50)

        common /arrays/ s(jd, kd, ld, 5), a(5, 5, md, md), b(5, 5, md, m
     *d), c(5, 5, md, md)
C
C
        do 200 j = ls, le
          do 200 m = 1, 5
            do 200 l = ls, le
              s(j, k, l, m) = s(j, k, l, m) - b(m, 1, j, l) * s(j - 1, k
     *, l, 1) - b(m, 2, j, l) * s(j + 1, k, l, 2) - b(m, 3, j, l) * s(j
     *- 1, k, l, 3) - b(m, 4, j, l) * s(j - 1, k, l, 4) - b(m, 5, j, l)
     ** s(j + 1, k, l, 5)
200     continue

	return
	end

