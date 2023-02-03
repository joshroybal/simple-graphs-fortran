module linkedlist
  implicit none
  ! linked list node type def'n
  type node
    integer :: n
    type (node), pointer :: next
  end type node
  contains
    ! function initializes linked list node
    function initnode(n) result(pnode)
      ! dummy arguments
      integer, intent(in) :: n
      ! function result location
      type (node), pointer :: pnode
      ! processing
      allocate (pnode)
      pnode%n = n
      nullify (pnode%next)
    end function initnode
    
    ! function returns length of linked list
    function listlength(head) result(n)
      ! dummy argument
      type (node), target, intent(in) :: head
      ! function result location
      integer :: n
      ! local data
      type (node), pointer :: curr
      ! processing
      curr => head
      n = 0
      do while (associated(curr) .eqv. .true.)
        n = n + 1
        curr => curr%next
      end do
    end function listlength

    ! function returns .true. if payload is in list, .false. otherwise
    function listfind(head, payload) result(val)
      ! dummy arguments
      type (node), target, intent(in) :: head
      integer, intent(in) :: payload
      ! function result location
      logical :: val
      ! local data
      type (node), pointer :: curr
      ! processing
      curr => head
      val = .false.
      do while (associated(curr) .eqv. .true.)
        if (curr%n .eq. payload) then
          val = .true.
          return
        end if
        curr => curr%next
      end do
    end function listfind

    ! function adds (pushes) node onto head node
    function listadd(head, n) result(pnode)
      ! dummy arguments
      integer, intent(in) :: n
      type (node), pointer :: head
      ! function result location
      type (node), pointer :: pnode
      ! processing
      nullify (pnode)
      pnode => initnode(n)
      pnode%next => head
    end function listadd

    ! function appends node to tail of list
    function listappend(head, payload) result(pnode)
      ! dummy argument
      integer, intent(in) :: payload
      type (node), pointer :: head
      ! function result location
      type (node), pointer :: pnode
      ! local data
      type (node), pointer :: curr
      ! processing
      nullify (pnode)
      nullify (curr)
      curr => head
      curr%next => head%next
      ! empty list - new head node
      if (associated(curr) .eqv. .false.) then
        deallocate (curr)
        pnode => initnode(payload)
        return
      end if
      do while (associated(curr%next) .eqv. .true.)
        curr => curr%next
      end do
      curr%next => initnode(payload)
      pnode => head
    end function listappend

    ! function inerts payloads into linked list maintaining ascending order
    function listinsert(head, payload) result(pnode)
      ! dummy arguments
      integer, intent(in) :: payload
      type (node), pointer :: head
      ! function result location
      type (node), pointer :: pnode
      ! local data
      type (node), pointer :: newnode, prev, curr
      ! processing
      nullify (pnode)
      nullify (newnode)
      nullify (prev)
      nullify (curr)
      curr => head
      ! create head location and return when empty
      if (associated(curr) .eqv. .false.) then
        pnode => initnode(payload)
        return
      end if
      if (payload .lt. head%n) then
        pnode => initnode(payload)
        pnode%next => head
        return
      end if
      prev => curr
      do while (associated(curr) .eqv. .true.)
        if (payload .lt. curr%n) then
          newnode => initnode(payload)
          prev%next => newnode
          newnode%next => curr
          pnode => head
          return
        end if
        prev => curr
        curr => curr%next
      end do
      ! new node at end
      newnode => initnode(payload)
      prev%next => newnode
      pnode => head
    end function listinsert

    ! function removes link containing payload from linked list
    function listremove(head, payload) result(pnode)
      ! dummy arguments
      integer, intent(in) :: payload
      type (node), pointer :: head
      ! function result location
      type (node), pointer :: pnode
      ! local data
      type (node), pointer :: prev, curr
      ! processing
      nullify (pnode)
      nullify (prev)
      nullify (curr)
      pnode => head
      curr => head
      ! empty list
      if (associated(curr) .eqv. .false.) return
      do while (associated(curr) .eqv. .true.)
        if (curr%n .eq. payload) then
          ! if prev is not associated curr%next is the new head node
          if (associated(prev) .eqv. .true.) then
            prev%next => curr%next
          else
            pnode => curr%next
          end if
          deallocate (curr)
          return
        end if
        prev => curr
        curr => curr%next
      end do
    end function listremove

    ! function reverses linked list
    function reverselist(head) result(pnode)
      ! dummy argument
      type (node), pointer :: head
      ! function result location
      type (node), pointer :: pnode
      ! local data
      type (node), pointer :: curr, prev, next
      ! processing
      nullify (pnode)
      nullify (curr)
      nullify (prev)
      nullify (next)
      curr => head
      do while (associated(curr) .eqv. .true.)
        next => curr%next
        curr%next => prev
        prev => curr
        curr => next
      end do
      pnode => prev
    end function reverselist
    
    ! function deletes every node in a linked list
    function deletelist(head) result(pnode)
      ! dummy argument
      type (node), pointer :: head
      ! function result location
      type (node), pointer :: pnode
      ! local data
      type (node), pointer :: tag
      ! processing
      nullify (pnode)
      nullify (tag)
      do while (associated(head) .eqv. .true.)
        tag => head
        head => head%next
        deallocate (tag)
      end do
    end function deletelist
    
    ! subroutine displays node payloads of linked list
    subroutine printlist(head)
      ! dummy argument
      type (node), pointer :: head
      ! local data
      character (len=12) :: nstr
      type (node), pointer :: curr
      ! processing
      nullify (curr)
      curr => head
      ! check if list is empty
      if (associated(curr) .eqv. .false.) then
        write (*,*)
        return
      end if
      do while (associated(curr%next) .eqv. .true.)
        write (nstr,*) curr%n
        write (*,'(a,'', '')',advance='no') trim(adjustl(nstr))
        curr => curr%next
      end do
      write (nstr,*) curr%n
      write (*,'(a)') trim(adjustl(nstr))
    end subroutine printlist

    ! subroutine writes dot file for graphviz box and pointer image
    subroutine dotlist(head, filename, title)
      ! dummy arguments
      type (node), pointer :: head
      character (len=*) :: filename, title
      ! local data
      integer :: n, i
      character (len=80) :: line
      character (len=12), allocatable, dimension(:) :: label
      type (node), pointer :: curr
      ! processing
      nullify (curr)
      curr => head
      if (associated(curr) .eqv. .false.) return
      n = listlength(head)
      allocate (label(n))
      i = 0
      do while (associated(curr) .eqv. .true.)
        i = i + 1
        write (label(i),*) curr%n
        label(i) = adjustl(label(i))
        curr => curr%next
      end do
      open (8,file=filename)
      write (8,*) 'digraph {'
      write (8,*) 'rankdir = LR;'
      write (8,*) 'node [ shape=record ];'
      write (8,*) 'edge [tailclip=false ];'
      write (8,*) trim(title)//' [ shape=box ]';
      do i = 1, n
        line=trim(label(i))//' [ label="{ <data> '//trim(label(i))&
          //' | <ref> }"];'
        write (8,*) trim(line)
      end do
      write (8,*) trim(title)//':e -> '//trim(label(1))
      do i = 1, n - 1
        line=trim(label(i))//':ref:c -> '//trim(label(i+1))&
          //':data [arrowhead=vee, arrowtail=dot, dir=both];'
        write (8,*) trim(line)
      end do
      write (8,*) '}'
      close (8)
      write (*,*) 'dot data written to file ', filename
      deallocate (label)
    end subroutine dotlist
end module linkedlist
