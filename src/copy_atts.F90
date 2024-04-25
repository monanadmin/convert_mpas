module copy_atts

   use netcdf
   use scan_input, only: input_handle_type, input_field_type
   use file_output, only: output_handle_type
   use remapper, only: target_field_type

contains

   ! Copy attributes from field to target_field

   integer function copy_field_atts(handle, field, output_handle, target_field) result(stat)

      implicit none

      type(input_field_type)   :: field
      type(input_handle_type)  :: handle
      type(output_handle_type) :: output_handle
      type(target_field_type)  :: target_field

      character(len=256) :: attname
      integer :: iatt
      integer :: natts
      integer :: target_varid

      stat = 0

      ! Inquire number of attributes - natts
      stat = nf90_inquire_variable(handle%ncid, field%varid, natts=natts)
      if (stat /= NF90_NOERR) then
         write (*, *) NF90_STRERROR(stat)
         stat = 1
         return
      end if

      ! Loop thru attributes
      do iatt = 1, natts
         ! Inquire attribute name - attname
         stat = nf90_inq_attname(handle%ncid, field%varid, iatt, attname)
         if (stat /= NF90_NOERR) then
            write (*, *) NF90_STRERROR(stat)
            stat = 1
            return
         end if
         ! Inquire variable ID of target_field - target_varid
         stat = NF90_INQ_VARID(output_handle%ncid, target_field%name, target_varid)
         if (stat /= NF90_NOERR) then
            write (*, *) NF90_STRERROR(stat)
            stat = 1
            return
         end if
         ! Copy attribute from field to target_field
         stat = nf90_copy_att(handle%ncid, field%varid, attname, output_handle%ncid, target_varid)
         if (stat /= NF90_NOERR) then
            write (*, *) NF90_STRERROR(stat)
            stat = 1
            return
         end if
      end do

   end function copy_field_atts

   ! Add attributes to lat or lon field
   integer function add_latlon_atts(handle) result(stat)

      implicit none

      type(output_handle_type) :: handle

      integer :: varid

      stat = 0

      stat = NF90_INQ_VARID(handle%ncid, 'latitude', varid)
      stat = NF90_PUT_ATT(handle%ncid, varid, 'units', 'degree_north')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'long_name', 'latitude')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'standard_name', 'latitude')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'axis', 'Y')

      stat = NF90_INQ_VARID(handle%ncid, 'longitude', varid)
      stat = NF90_PUT_ATT(handle%ncid, varid, 'units', 'degree_east')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'long_name', 'longitude')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'standard_name', 'longitude')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'axis', 'X')

   end function add_latlon_atts

   ! Add attributes to lat or lon field
   integer function add_time_atts(handle, Time_Label) result(stat)

      implicit none

      type(output_handle_type) :: handle
      character(LEN=*), intent(in) :: Time_Label
      integer :: varid

      stat = 0

      stat = NF90_INQ_VARID(handle%ncid, 'Time', varid)
      stat = NF90_PUT_ATT(handle%ncid, varid, 'units', trim(Time_Label))
      stat = NF90_PUT_ATT(handle%ncid, varid, 'calendar', 'proleptic_gregorian')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'standard_name', 'time')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'axis', 'T')

   end function add_time_atts

   ! Add attributes to lat or lon field
   integer function add_zlevels_atts(handle) result(stat)

      implicit none

      type(output_handle_type) :: handle

      integer :: varid

      stat = 0

      stat = NF90_INQ_VARID(handle%ncid, 'level', varid)
      stat = NF90_PUT_ATT(handle%ncid, varid, 'standard_name', 'air_pressure')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'long_name', 'Levels for vertical interpolation of winds to isobaric surfaces')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'units', 'Pa')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'positive', 'down')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'axis', 'Z')

      !stat = NF90_INQ_VARID(handle%ncid, 'u_iso_levels', varid)
      !stat = NF90_PUT_ATT(handle % ncid, varid, 'axis', 'Z')

      !stat = NF90_INQ_VARID(handle%ncid, 'z_iso_levels', varid)
      !stat = NF90_PUT_ATT(handle % ncid, varid, 'axis', 'Z')

   end function add_zlevels_atts

   ! Add attributes to lat or lon field
   integer function add_zlevels_model_atts(handle) result(stat)

      implicit none

      type(output_handle_type) :: handle

      integer :: varid

      stat = 0

      stat = NF90_INQ_VARID(handle%ncid, 'level', varid)
      stat = NF90_PUT_ATT(handle%ncid, varid, 'standard_name', 'index_model')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'long_name', 'Levels for vertical interpolation of winds to isobaric surfaces')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'units', 'index')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'positive', 'up')
      stat = NF90_PUT_ATT(handle%ncid, varid, 'axis', 'Z')

   end function add_zlevels_model_atts

end module copy_atts
