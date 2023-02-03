program driver
  use graph
  implicit none
  ! variables and arrays
  integer :: i, n, m
  integer, allocatable, dimension(:) :: vertices
  integer, allocatable, dimension(:,:) :: edges, matrix
  type (arrayp), allocatable, dimension(:) :: adjlst
  ! processing
  
  write (*,*) 'enter no. of vertices'
  read (*,*) n
  allocate (vertices(n))
  do i = 1, n
    vertices(i) = i
  end do
  
  write (*,*) 'enter no. of edges'
  read (*,*) m
  allocate (edges(2,m))
  do i = 1, m
    read (*,*) edges(1,i), edges(2,i)
  end do
 
  write (*,'(5i2)') vertices
  write (*,'(10(2i2,1x))') edges

  allocate (matrix(n,n))
  matrix = adjacencymatrix(n, m, edges)
  call printmatrix(n, matrix)

  allocate (adjlst(n))
  adjlst = adjacencylist(n, m, vertices, edges)
  call printlists(n, adjlst)

  ! write dot representation of graph to .gv file
  call dotgraph(m, edges, 'graph.gv')

  ! write dot representation of adjacency list to .gv file
  call dotlists(n, adjlst)

  do i = 1, n
    adjlst(i)%p => deletelist(adjlst(i)%p)
  end do
  deallocate (adjlst)
  deallocate (matrix)
  deallocate (edges)
  deallocate (vertices)
end program
