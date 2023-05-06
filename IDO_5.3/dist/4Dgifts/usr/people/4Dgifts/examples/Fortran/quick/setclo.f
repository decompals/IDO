       subroutine setclo(gid) 
c 
       integer gid
c
       call gflush
       call winset(gid)
       call cls
       call winclo(gid)
       call gexit
       return
       end
