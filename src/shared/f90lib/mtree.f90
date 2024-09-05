!------------------------------------------------------------------------------
!                 Module for implementing a tree structure
!
!------------------------------------------------------------------------------
module mtree
	private
	public init_mtree
	public close_mtree
	public add_mtree
	public list_elements
	
	type Type_Element
		integer		::father
		integer		::left_brother
		integer         ::first_son
		character(len=4)::value
	end type
	type(type_element),allocatable::element(:)
	integer::nelements=0
	integer::p
	integer::p_element=0
	contains
!-------------------------------------------------------------------------------
! init
!------------------------------------------------------------------------------	
	subroutine init_mtree(maxsize)
		allocate(element(1:maxsize))
		nelements=0
		p_element=0
	end subroutine
!-------------------------------------------------------------------------------
! close
!-------------------------------------------------------------------------------
	subroutine close_mtree 
		deallocate(element)
	end subroutine
!-----------------------------------------------------------------------------
! add ELEMENT
!------------------------------------------------------------------------------
	subroutine add_mtree(value_in)
		character(len=*),intent(in)::value_in 
		! Add root (first father)
		if (nelements==0) then
			nelements=1
			
			element(nelements)%father=0
			element(nelements)%left_brother=0
			element(nelements)%first_son=0
			p_element=nelements
			element(p_element)%value=trim(value_in)
			return 
		end if
		
		! Add first son of the root
		if (nelements==1) then
			nelements=nelements+1
			element(nelements)%father=p_element
			if (element(p_element)%first_son==0) element(p_element)%first_son=nelements
			p_element=nelements
			element(p_element)%value=trim(value_in)
			element(p_element)%left_brother=0
			element(p_element)%first_son=0
			return
		end if
		
		
	end subroutine

	!-----------------------------------------------------------------------------
	! subroutine list ELEMENTS for tree
	!------------------------------------------------------------------------------
	subroutine list_elements
		integer::a,i
		i=1
		print *,"ELEMET=",p_element,nelements
		a=list_element(i)
		
	end subroutine

	!-----------------------------------------------------------------------------
	! function list next element for tree (recursive function)
	!------------------------------------------------------------------------------	
	recursive function list_element(i) result(p)
		integer,intent(in)::i
		integer::p_son,p
		
		print *,element(i)%value
		p_son=element(i)%first_son
		if ((p_son>0).and.(i<=nelements)) then 
			p=list_element(p_son)
		end if
	end function	
		

end module