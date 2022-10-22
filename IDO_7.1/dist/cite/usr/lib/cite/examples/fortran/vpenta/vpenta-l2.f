      subroutine vpenta(jl, ju)
        implicit double precision (a-h, o-z)
        parameter (nja = 128, njb = 128, kl = 1, ku =
     *128)
        common /arrays/ a(nja, njb), b(nja, njb), c(nja, njb), d(nja, nj
     *b), e(nja, njb), f(nja, njb, 3), x(nja, njb), y(nja, njb), fx(nja,
     * njb, 3)
        do 4 jx = ju - 2, jl, -1
          do 15 k = jl, ju
            f(jx, k, 1) = f(jx, k, 1) - x(jx, k) * f(jx + 1, k, 1) - y(j
     *x, k) * f(jx + 2, k, 1)
            f(jx, k, 2) = f(jx, k, 2) - x(jx, k) * f(jx + 1, k, 2) - y(j
     *x, k) * f(jx + 2, k, 2)
            f(jx, k, 3) = f(jx, k, 3) - x(jx, k) * f(jx + 1, k, 3) - y(j
     *x, k) * f(jx + 2, k, 3)
15        continue
4       continue

	return
	end
