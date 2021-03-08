# programme

program projet
use m_type
implicit none
type (mes) :: M
real, dimension(10,10)::C

call VTSWriter(0,0,M%NX+1,M%Ny+1,M%Xn,M%Yn,C,M%un,M%vn,'ini')
call lecture_donnees(M)

allocate(M%Xn(M%Nx+1, M%Ny+1))
allocate(M%Xv(M%Nx, M%Ny))
allocate(M%Yn(M%Nx+1, M%Ny+1))
allocate(M%Yv(M%Ny, M%Nx))

call maillage(M%Un, M%Uv)
call VTSWriter(0,0,M%NX+1,M%Ny+1,M%Xn,M%Yn,C,M%un,M%vn,'int')

allocate(M%Uv(M%Nx, M%Ny))
allocate(M%Vv(M%Nx, M%Ny))
allocate(M%Un(M%Nx+1, M%Ny+1))
allocate(M%Vn(M%Nx+1, M%Ny+1))
allocate(M%delta_y(M%Nx, M%Ny))
allocate(M%d_t(M%Nx, M%Ny))

call vitesses(M)
!call delta_temps(M)
call Concentration (C)

deallocate (M%Xn)
deallocate (M%Xv)
deallocate (M%Yn)
deallocate (M%Yv)
deallocate (M%Un)
deallocate (M%Uv)
deallocate (M%Vn)
deallocate (M%Vv)
deallocate (M%delta_y)
deallocate (M%d_t)

call VTSWriter(0,0,M%NX+1,M%Ny+1,M%Xn,M%Yn,C,M%un,M%vn,'end')

end program projet
