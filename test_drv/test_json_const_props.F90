program test_const_props
  use const_props_mod, only: const_props_type
  use json_module

  implicit none

  type(json_file) :: json       !! the JSON structure read from the file
  type(json_value),pointer :: p !! a pointer for low-level manipulations
  type(json_core) :: core       !! factory for manipulating `json_value` pointers
  type(json_value),pointer :: child 
  type(json_value),pointer :: child2
  character(len=:),allocatable :: name

  character(len=*), parameter :: inputfile = '../inputs/chem_info.json'

  type(const_props_type), allocatable :: cnst_info(:)

  integer :: ncnst, n
  logical :: found

  character(len=:),allocatable :: string
  real(8) :: rval

  write(*,*) 'BEGIN TEST'

  call json%initialize()

  write(*,*) 'Load the file :'//inputfile

  call json%load_file(filename = inputfile)
  
  call json%print_file()

  call core%initialize()
  call json%get(p) ! get root

  call core%get_child(p,child)
  call core%info(child,name=name)

  write(*,*)  'Read obj data : '//name

  deallocate(name)
  
  ncnst = core%count(child)

  write(*,*) '  ncnst = ',ncnst

  allocate(cnst_info(ncnst))

  do n = 1,ncnst
     call core%get_child(child, n, child2, found)
     if (found) then
        call core%get(child2,'name',string)
        call cnst_info(n)%set_name(string)
        deallocate(string)
        
        call core%get(child2,'description',string)
        call cnst_info(n)%set_desc(string)
        deallocate(string)

        call core%get(child2,'molecweight',rval)
        call cnst_info(n)%set_wght(real(rval))

        call cnst_info(n)%print()
     else
        write(*,*) ' ERROR: Did not find child ',n
        call abort()
     endif
  enddo

  call json%destroy()

  write(*,*) 'END TEST'

end program test_const_props

