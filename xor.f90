program xor3
	implicit none
	!changed the dimension sizes for three inputs
	real, dimension(1:11) :: w
	real, dimension(1:8,1:4) :: t
	real, dimension(4:6) :: delta
	real, dimension(4:6) :: o
	real, dimension(1:8) :: error
	real :: alpha = 0.1, k = 2.0
	integer :: i, iter
	
	
  interface
    real function f(k,net)
      real, intent(in) :: net, k
    end function f
  end interface
	
	
    !truth table for an xor gate with tree inputs
	t(1,:) = (/ 0.0, 0.0, 0.0, 0.0 /)
	t(2,:) = (/ 0.0, 0.0, 1.0, 1.0 /)
	t(3,:) = (/ 0.0, 1.0, 0.0, 1.0 /)
	t(4,:) = (/ 0.0, 1.0, 1.0, 0.0 /)
	t(5,:) = (/ 1.0, 0.0, 0.0, 1.0 /)
	t(6,:) = (/ 1.0, 0.0, 1.0, 0.0 /)
	t(7,:) = (/ 1.0, 1.0, 0.0, 0.0 /)
	t(8,:) = (/ 1.0, 1.0, 1.0, 1.0 /)
	
	
	write(6,*)'This program calculates the truth table for a system with'
	write(6,*)'three inputs.'
	write(6,*)'It does this with a neural netwoork, which has 3 input nodes'
	write(6,*)'2 hidden nodes and then one output node.'
	
 ! set random weights
  call random_number(w)
 
  ! loop over some iterations
  do iter=1,10000
 
    ! train on all sets of input data
    do i=1,8
      ! calculate all outputs
      o(4) = f(k,w(1)+w(4)*t(i,1)+w(6)*t(i,2)+w(8)*t(i,3))
      o(5) = f(k,w(2)+w(5)*t(i,1)+w(7)*t(i,2)+w(9)*t(i,3))
      o(6) = f(k,w(3)+w(10)*o(4)+w(11)*o(5))
      ! calculate deltas for output node (delta(4)) then hidden nodes (delta(5)
	  !and delta(6))
      delta(6) = k*o(6)*(1.0-o(6))*(t(i,4)-o(5))
      delta(5) = k*o(5)*(1.0-o(5))*delta(6)*w(11)
      delta(4) = k*o(4)*(1.0-o(4))*delta(6)*w(10)
      ! now actually update the weights
      w(1) = w(1) + alpha * delta(4)
      w(2) = w(2) + alpha * delta(5)
      w(3) = w(3) + alpha * delta(6)
      w(4) = w(4) + alpha * delta(4) * t(i,1)
      w(5) = w(5) + alpha * delta(5) * t(i,1)
      w(6) = w(6) + alpha * delta(4) * t(i,2)
      w(7) = w(7) + alpha * delta(5) * t(i,2)
      w(8) = w(8) + alpha * delta(4) * t(i,3)
      w(9) = w(9) + alpha * delta(5) * t(i,3)
	  w(10) = w(10) + alpha * delta(6) * o(4)
	  w(11) = w(10) + alpha * delta(6) * o(5)
   
    end do

  end do
 

 
  ! write out truth table
 write(6,*)'The truth table output is:'
  do i=1,8
    ! calculate all outputs
	!updated final outputs
      o(4) = f(k,w(1)+w(4)*t(i,1)+w(6)*t(i,2)+w(8)*t(i,3))
      o(5) = f(k,w(2)+w(5)*t(i,1)+w(7)*t(i,2)+w(9)*t(i,3))
      o(6) = f(k,w(3)+w(10)*o(4)+w(11)*o(5))
    write(6,*) t(i,1), t(i,2), t(i,3), t(i,4)
  end do
end program xor3	
	
!------------------------------------------------------------------------------	
	!the function that calculates the cost of the 
	!city to city trip
	!net is net input
	function f(k,net)
		implicit none
		real, intent(in) :: net, k
		real :: f
		f = 1.0/(1.0+exp(-k*net))
	end function f
	