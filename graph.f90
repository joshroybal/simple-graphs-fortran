module graph
  use linkedlist
  implicit none
  ! array of pointers type def'n
  type arrayp
    type (node), pointer :: p
  end type arrayp  
  contains
    ! function returns adjacency matrix of graph
    function adjacencymatrix(n, m, edges) result(matrix)
      ! dummy arguments
      integer, intent(in) :: n, m
      integer, dimension(2,m), intent(in) :: edges
      ! function result location
      integer, dimension(n,n) :: matrix
      ! local data
      integer :: i
      ! processing
      matrix = 0
      do i = 1, m
        matrix ( edges(1,i), edges(2,i) ) = 1
        matrix ( edges(2,i), edges(1,i) ) = 1
      end do
    end function adjacencymatrix

    ! function returns adjacency list of graph
    function adjacencylist(n, m, vertices, edges) result(listarr)
      ! dummy arguments
      integer, intent(in) :: n, m
      integer, dimension(n), intent(in) :: vertices
      integer, dimension(2,m), intent(in) :: edges
      ! function result location
      type (arrayp), dimension(n) :: listarr
      ! local data
      integer :: i, j
      ! processing
      do i = 1, n
        nullify (listarr(i)%p)
        do j = 1, m
          if (vertices(i) .eq. edges(1,j))& 
            listarr(i)%p => listinsert(listarr(i)%p, edges(2,j))
          if (vertices(i) .eq. edges(2,j))&
            listarr(i)%p => listinsert(listarr(i)%p, edges(1,j))
        end do
      end do
    end function adjacencylist

    ! subroutine pretty prints vertices
    subroutine printvertices(n, v)
      ! dummy arguments
      integer, intent(in) :: n
      integer, dimension(n), intent(in) :: v
      ! local data
      integer :: i
      character (len=12) :: nstr
      ! processing
      write (*,'(a)',advance='no') '{'
      do i = 1, n
        write (nstr,*) v(i)
        nstr = adjustl(nstr)
        write (*,'(a)',advance='no') trim(nstr)
        if (i .lt. n) write (*,'(a)',advance='no') ','
      end do
      write (*,'(a)') '}'
    end subroutine printvertices

    ! subroutine pretty prints edges
    subroutine printedges(m, e)
      ! dummy arguments
      integer, intent(in) :: m
      integer, dimension(2,m), intent(in) :: e
      ! local data
      integer :: j
      character (len=12) :: n1, n2
      ! processing
      write (*,'(a)',advance='no') '{'
      do j = 1, m
        write (n1,*) e(1,j)
        write (n2,*) e(2,j)
        n1 = adjustl(n1)
        n2 = adjustl(n2)
        write (*,'(a)',advance='no') '{'//trim(n1)//','//trim(n2)//'}'
        if (j .lt. m) write (*,'(a)',advance='no') ','
      end do
      write (*,'(a)') '}'
    end subroutine printedges

    ! subroutine pretty prints adjacency matrix
    subroutine printmatrix(n, matrix)
      ! dummy arguments
      integer, intent(in) :: n
      integer, dimension(n,n), intent(in) :: matrix
      ! local data
      character (len=12) :: nstr, fmtstr
      ! processing
      write (nstr,*) n
      fmtstr = '(' // trim(adjustl(nstr)) // 'i2)'
      write (*,fmtstr) matrix
    end subroutine printmatrix

    ! subroutine pretty prints adjacency list
    subroutine printlists(n, lists)
      ! dummy arguments
      integer, intent(in) :: n
      type (arrayp), dimension(n), intent(in) :: lists
      ! local data
      integer :: i
      ! processing
      do i = 1, n
        write (*,1000,advance='no') i
        1000 format (1x,i1,': ')
        call printlist(lists(i)%p)
      end do
    end subroutine printlists

    ! subroutine writes graphviz .gv dot file
    subroutine dotgraph(m, edges, filename)
      ! dummy arguments
      integer, intent(in) :: m
      integer, dimension(2,m), intent(in) :: edges
      character (len=*), intent(in) :: filename
      ! local data
      integer :: i
      ! processing
      open (8,file=filename)
      write (8,*) 'graph {'
      write (8,*) 'layout = circo;'
      ! write (8,*) 'node [ shape=point ]'
      do i = 1, m
        write (8,*) edges(1,i), '--', edges(2,i)
      end do
      write (8,*) '}'
      close (8)
      write (*,*) 'dot data written to file ', filename
    end subroutine dotgraph

    ! subroutine writes adjacency list as a series of box and pointer style
    ! dot files which can later be combined by the convert tool
    ! e.g convert 1.jpeg 2.jpeg -append adjlist.jpeg
    subroutine dotlists(n, lists)
      ! dummy arguments
      integer, intent(in) :: n
      type (arrayp), dimension(n), intent(in) :: lists
      ! local data
      integer :: i
      character (len=12) :: nstr
      character (len=50) :: outfile
      ! processing
      do i = 1, n
        write (nstr,*) i
        nstr = adjustl(nstr)
        outfile = 'list'//trim(nstr)//'.gv'
        call dotlist(lists(i)%p, outfile, nstr)
      end do
    end subroutine dotlists
end module graph
