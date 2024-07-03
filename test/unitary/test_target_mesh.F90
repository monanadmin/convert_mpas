
program test_target_mesh
   !! # Tests bug fix for not generating correctly first and last lat and lon values
   !!
   !! Author: Denis Eiras
   !!
   !!E-mail: <mailto:denis.eiras@inpe.br>  
   !!
   !!Date: 03/07/2024
   !!
   !!#####Version: -
   !!
   !! —
   !!**When nlat = 360 nlon = 720 startlat = -90 startlon = 0 endlat = 90 endlon = 360 in target domain the fisrt and last values in output file is not the same: 
   !!
   !!
   !!** History**:
   !!
   !!- 03/07/2024 - creation
   !!—
   !!<img src="https://www.gnu.org/graphics/gpl
   use target_mesh

   implicit none
   
   type(target_mesh_type) :: mesh
   integer :: i, j
   integer :: nLat_expected, nLon_expected
   real :: startLat_expected, endLat_expected
   real :: startLon_expected, endLon_expected
   real, parameter :: pi_const = 2.0*asin(1.0)
   integer :: ret_stat
   
   ! Mock input data
   nLat_expected = 360
   nLon_expected = 720
   startLat_expected = -90.0
   endLat_expected = 90.0
   startLon_expected = -180.0
   endLon_expected = 180.0
   
   
   ! Test with 2D arrays provided
   ret_stat = target_mesh_setup(mesh)
   
   ! Check if mesh values are correctly set
   if (.not. mesh%valid) then
      print *, "Test failed: mesh not marked as valid"
      return
   end if
   
   if (mesh%nLat /= nLat_expected) then
      print *, "Test failed: nLat not correctly set"
      return
   end if
   
   if (mesh%nLon /= nLon_expected) then
      print *, "Test failed: nLon not correctly set"
      return
   end if
   
   ! Check first and last values of mesh%lats and mesh%lons
   if (abs(mesh%lats(1, 1) - startLat_expected * pi_const / 180.0) > 1.0E-6) then
      print *, "Test failed: First value of mesh%lats incorrect"
      return
   end if
   
   if (abs(mesh%lats(1, mesh%nLat) - endLat_expected * pi_const / 180.0) > 1.0E-6) then
      print *, "Test failed: Last value of mesh%lats incorrect"
      return
   end if
   
   if (abs(mesh%lons(1, 1) - startLon_expected * pi_const / 180.0) > 1.0E-6) then
      print *, "Test failed: First value of mesh%lons incorrect"
      return
   end if
   
   if (abs(mesh%lons(mesh%nLon, 1) - endLon_expected * pi_const / 180.0) > 1.0E-6) then
      print *, "Test failed: Last value of mesh%lons incorrect"
      return
   end if

   if (ret_stat /= 0) then
      print *, "Test failed: Return status not 0"
      return
   end if
   
   print *, "All tests passed successfully!"


end program test_target_mesh
