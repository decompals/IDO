      subroutine vpenta(jl, ju)
        implicit double precision (a-h, o-z)
        parameter (nja = 128, njb = 128, kl = 1, ku =
     *128)
        common /arrays/ a(nja, njb), b(nja, njb), c(nja, njb), d(nja, nj
     *b), e(nja, njb), f(nja, njb, 3), x(nja, njb), y(nja, njb), fx(nja,
     * njb, 3)
        do 3 j = jl + 2, ju - 2
          do 11 k = kl, ku
            rld2 = a(j, k)
            rld1 = b(j, k) - rld2 * x(j - 2, k)
            rld = c(j, k) - (rld2 * y(j - 2, k) + rld1 * x(j - 1, k))
            rldi = 1.d0 / rld
            f(j, k, 1) = (f(j, k, 1) - rld2 * f(j - 2, k, 1) - rld1 * f(
     *j - 1, k, 1)) * rldi
            f(j, k, 2) = (f(j, k, 2) - rld2 * f(j - 2, k, 2) - rld1 * f(
     *j - 1, k, 2)) * rldi
            f(j, k, 3) = (f(j, k, 3) - rld2 * f(j - 2, k, 3) - rld1 * f(
     *j - 1, k, 3)) * rldi
            x(j, k) = (d(j, k) - rld1 * y(j - 1, k)) * rldi
            y(j, k) = e(j, k) * rldi
11        continue
3       continue
	return
	end
