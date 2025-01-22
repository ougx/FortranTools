
!
! MAIN PROGRAM HERE.  This is just an example so you know
! how to use it.   This file is in the public domain.
! 

program kd_tree_test
  use kdtree2_module

  integer :: n, d
  
  real(kdkind), dimension(:,:), allocatable :: my_array
  real(kdkind), allocatable :: query_vec(:)

  type(kdtree2), pointer :: tree, tree2, tree3
  integer                :: i, ii
  ! this is how you declare a tree in your main program
  
  n = 134828
  d = 3
  open(file='p:\1879 - CA-DWR Basin Characterization Project\Analysis\Inputs_PPSGS\data_combined.csv', unit=10)
  read(10, *)
  
  allocate(my_array(d, n))
  do i=1, n
    read(10, *) ii, my_array(:,i)
  end do
  
  print*, my_array(:, 1)
  print*, my_array(:, n)
  
  print*, "Tree build started!"
  tree => kdtree2_create(my_array, sort=.false., rearrange=.false.)
  print*, "Tree built!"
end program kd_tree_test
