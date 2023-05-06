      program shmmem

        logical ptr, shmget
        integer i, j, pid, fork, wait
        external shmget, shmfree

        ptr = shmget()
        pid = fork()
        if (pid .eq. 0) then
            call sleep(1)
            call readsmem(%val(ptr))
        else
            call writesmem(%val(ptr))
            j = wait(i)
            call shmfree(%val(ptr))
        endif
      end


      subroutine writesmem (ptr)

        structure /shared/
            real x, y, xy
            character*25 ch
        end structure
        record /shared/ ptr

        ptr.x     = 22.0
        ptr.y     =  7.0
        ptr.xy    = ptr.x/ptr.y
        ptr.ch    = 'Shared memory values: '
        return
      end


      subroutine readsmem (ptr)

        structure /shared/
            real x, y, xy
            character*25 ch
        end structure
        record /shared/ ptr

        print *,  ptr.ch, ptr.x, ptr.y, ptr.xy
        return
      end
