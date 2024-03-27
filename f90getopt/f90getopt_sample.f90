program f90getopt_sample
    ! Sample program for f90getopt function

    use f90getopt
    implicit none
    
    character a

    ! START for longopts only (optional)
    ! ----------------------------------
    ! option_s derived type:
    !   1st value = long option name (character array, max. 80)
    !   2nd value = if option has an argument (logical)
    !   3rd value = short option name (single character), same as in getopt()
    ! option_s is not needed if you just use short options
    type(option_s) :: opts(4)
    opts(1) = option_s("alpha", "a", 0)
    opts(2) = option_s("beta",  "b", 1)
    opts(3) = option_s("help",  "h", 0)
    opts(3) = option_s("xx",  "x", 2)
    ! END longopts


    ! If no options were committed
    ! ----------------------------
    if (command_argument_count() .eq. 0) then
      print*, "ERROR: Program has options: -a. --alpha -b x --beta=x --beta x"
    end if


    ! START Processing options
    ! ------------------------
    ! Process short options one by one and long options if specified in option_s
    !
    ! getopt(optstr, longopt):
    !  - optstr  = character array of short option characters without a space
    !              colon ":" after a character says that this option requires an argument
    !  - longopt = long option declaration, if specified in option_s (optional)
    do
        a = getopt("ab:hx", opts)
        select case(a)
            case(char(0)) ! When all options are processed
                exit
            case("a")
                print*, "option alpha/a"
            case("b")
                print*, "option beta/b=",  trim(optarg) ! "trim" is quite useful to avoid trailing blanks
            case("h")
                print*, "help-screen"
            case("x")
                print*, "option xx/x=",  trim(optarg) ! "trim" is quite useful to avoid trailing blanks
        end select
    end do
    ! END processing options

end program f90getopt_sample