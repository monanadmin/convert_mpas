program convert_mpas

    use copy_atts
    use scan_input
    use mpas_mesh
    use target_mesh
    use remapper
    use file_output
    use field_list
    use timer

    implicit none

    ! Timers
    type (timer_type) :: total_timer, &
                         read_timer, &
                         remap_timer, &
                         write_timer

    integer :: stat
    character (len=1024) :: mesh_filename, data_filename,Time_Label,filename
    type (mpas_mesh_type) :: source_mesh
    type (target_mesh_type) :: destination_mesh
    type (input_handle_type) :: handle
    type (input_field_type) :: field
    type (remap_info_type) :: remap_info
    type (output_handle_type) :: output_handle
    type (target_field_type) :: target_field
    type (field_list_type) :: include_field_list, exclude_field_list

    integer :: iRec,ios
    integer :: nRecordsIn, nRecordsOut
    integer :: iFile
    integer :: fileArgStart, nArgs
    logical :: exists        
    integer :: nVertLevels  = 55
    integer :: nOznLevels   = 59
    integer :: nMonths      = 12
    integer :: nSoilLevels  =  4
    integer :: nIsobaricLev = 27
    character (len=20) :: verticalCoord=''
    NAMELIST /config_convert_mpas/ verticalCoord,nVertLevels,nOznLevels,nMonths,nSoilLevels,nIsobaricLev

    call timer_start(total_timer)

    if (command_argument_count() < 1) then
        write(0,*) ' '
        write(0,*) 'Usage: convert_mpas mesh-file [data-files]'
        write(0,*) ' '
        write(0,*) 'If only one file argument is given, both the MPAS mesh information and'
        write(0,*) 'the fields will be read from the specified file.'
        write(0,*) 'If two or more file arguments are given, the MPAS mesh information will'
        write(0,*) 'be read from the first file and fields to be remapped will be read from'
        write(0,*) 'the subsequent files.'
        write(0,*) 'All time records from input files will be processed and appended to'
        write(0,*) 'the output file.'
        stop 1
    end if

    nArgs = command_argument_count()

    call get_command_argument(1, mesh_filename)
    if (nArgs == 1) then
        fileArgStart = 1
    else 
        fileArgStart = 2
    end if

    write(0,*) 'Reading MPAS mesh information from file '''//trim(mesh_filename)//''''
   
    inquire(file='convert_mpas.nml', exist=exists)
    if (exists) then
       write(0,*) ' '
       write(0,*) 'Reading target convert_mpas.nml specification from file ''configuration'''

       open (unit=51, file='./convert_mpas.nml', &
         form='formatted', access='sequential', &
         action='read', status='old', iostat=ios)
       if (ios /= 0) then
          write (unit=0, fmt='(3a,i4)') &
            ' ** (error) ** open file ', &
              './'//'./convert_mpas.nml', &
            ' returned iostat = ', ios
          stop  ' ** (error 1) **'
       end if
       read  (unit=51, nml=config_convert_mpas)
       close (unit=51)
       write(0,*) 'need convert_mpas.nml file '
       write (unit=0, fmt='(/,a)')  ' &config_convert_mpas'
       write (unit=0, fmt='(a,a12)') '    verticalCoord = ', 'MPAS_Model'   !  'Pressure'
       write (unit=0, fmt='(a,/)')  ' /'
    else
       write(0,*) ' '
       write(0,*) 'need convert_mpas.nml file '
       write (unit=0, fmt='(/,a)')  ' &config_convert_mpas'
       write (unit=0, fmt='(a,a12)') '    verticalCoord = ', 'MPAS_Model'   !  'Pressure'
       write (unit=0, fmt='(a,/)')  ' /'
       stop  ' ** (error 2) **'
    endif 
    !
    ! Generate the target grid  target_mesh_setup
    !
    ! Try to parse nLat, nLon from target_domain file
    !
    !
    if (target_mesh_setup(destination_mesh) /= 0) then
        write(0,*) 'Error: Problems setting up target mesh'
        stop 2
    end if

    !
    ! Get information defining the MPAS mesh cellsOnCell
    !
    if (mpas_mesh_setup(mesh_filename, source_mesh) /= 0) then
        write(0,*) 'Error: Problems setting up MPAS mesh from file '//trim(mesh_filename)
        stat = target_mesh_free(destination_mesh)
        stop 3
    end if
    !
    ! Compute weights for mapping from MPAS mesh to target grid
    !
    write(0,*) ' '
    write(0,*) 'Computing remapping weights'
    call timer_start(remap_timer)
    if (remap_info_setup(source_mesh, destination_mesh, remap_info) /= 0) then
        write(0,*) 'Error: Problems setting up remapping'
        stat = mpas_mesh_free(source_mesh)
        stat = target_mesh_free(destination_mesh)
        stop 4
    end if
    call timer_stop(remap_timer)
    write(0,'(a,f10.6,a)') '    Time to compute remap weights: ', timer_time(remap_timer), ' s'

    !
    ! Open output file
    !
    if (file_output_open('latlon.nc', output_handle, mode=FILE_MODE_APPEND, nRecords=nRecordsOut) /= 0) then
        write(0,*) 'Error: Problems opening output file'
        stat = mpas_mesh_free(source_mesh)
        stat = target_mesh_free(destination_mesh)
        stat = remap_info_free(remap_info)
        stop 5
    end if

    if (nRecordsOut /= 0) then
        write(0,*) 'Existing output file has ', nRecordsOut, ' records'
    else
        write(0,*) 'Created a new output file'
    end if

    !
    ! Get list of fields to include or exclude from input file
    !
    stat = field_list_init(include_field_list, exclude_field_list)


    !
    ! Loop over input data files
    !
    do iFile=fileArgStart,nArgs

        call get_command_argument(iFile, data_filename)

        write(0,*) 'Remapping MPAS fields from file '''//trim(data_filename)//''''
        write(0,*) 'from file '''//data_filename(1:4)//''''
        filename=get_filename(data_filename)
        if(trim(filename(1:4)) == 'diag')then
        write(0,*) ' MPAS fields from file '''//data_filename(1:4)//''''
           Time_Label='hours since 2021-01-01 00:00:00'
           Time_Label(13:16)=filename(6:9)
           Time_Label(18:19)=filename(11:12)
           Time_Label(21:22)=filename(14:15)
           Time_Label(24:25)=filename(17:18)
           Time_Label(27:28)=filename(20:21)
           Time_Label(30:31)=filename(23:24)
        else if(trim(filename(1:7)) == 'history')then
        write(0,*) ' MPAS fields from file '''//data_filename(1:7)//''''
           Time_Label='hours since 2021-01-01 00:00:00'
           Time_Label(13:16)=filename(9:12)
           Time_Label(18:19)=filename(14:15)
           Time_Label(21:22)=filename(17:18)
           Time_Label(24:25)=filename(20:21)
           Time_Label(27:28)=filename(23:24)
           Time_Label(30:31)=filename(26:27)
        else           
	   Time_Label='hours since 2021-01-01 00:00:00'
           write(0,*) 'Error: Problems opening input file pk=>'//trim(data_filename)
           stop
        endif
        !
        ! Open input data file
        !
        if (scan_input_open(data_filename, handle, nRecords=nRecordsIn) /= 0) then
            write(0,*) 'Error: Problems opening input file '//trim(data_filename)
            write(0,*) '       This could result from an input file with no unlimited dimension.'
            stat = file_output_close(output_handle)
            stat = scan_input_close(handle)
            stat = mpas_mesh_free(source_mesh)
            stat = target_mesh_free(destination_mesh)
            stat = remap_info_free(remap_info)
            stop 6
        end if
    
        write(0,*) 'Input file has ', nRecordsIn, ' records'
    
        ! generally, we should make sure dimensions match in existing output files
        ! and in subsequent MPAS input files
    
        !
        ! Scan through input file, determine which fields will be remapped,
        ! and define those fields in the output file; this only needs to be done
        ! if there are no existing records in the output file (i.e., the output
        ! file is a new file)
        !
        write(0,*) ' '
        if (nRecordsOut == 0) then
            write(0,*) 'Defining fields in output file'

            ! Define 'lat' and 'lon' fields for target mesh
            stat = remap_get_target_latitudes(remap_info, target_field)
            stat = file_output_register_field(output_handle, target_field)
            stat = free_target_field(target_field)

            stat = remap_get_target_longitudes(remap_info, target_field)
            stat = file_output_register_field(output_handle, target_field)
            stat = free_target_field(target_field)


            if(TRIM(verticalCoord) == 'Pressure')then
               stat = remap_get_target_t_iso_levels(remap_info, target_field,nIsobaricLev)
               stat = file_output_register_field(output_handle, target_field)
               stat = free_target_field(target_field)

               stat = remap_get_target_u_iso_levels(remap_info, target_field,nIsobaricLev)
               stat = file_output_register_field(output_handle, target_field)
               stat = free_target_field(target_field)
            else if(TRIM(verticalCoord) == 'MPAS_Model')then   
               stat = remap_get_target_nVertLevels(remap_info, target_field,nVertLevels)
               stat = file_output_register_field  (output_handle, target_field)
               stat = free_target_field(target_field)
            else
               write(0,*) 'error at verticalCoord=',TRIM(verticalCoord)
               stop 66
            endif

            stat = remap_get_target_time(remap_info, target_field)
            stat = file_output_register_field(output_handle, target_field)
            stat = free_target_field(target_field)

            do while (scan_input_next_field(handle, field) == 0) 
                if (can_remap_field(field) .and. &
                    should_remap_field(field, include_field_list, exclude_field_list)) then
                    if(TRIM(verticalCoord) == 'Pressure')then
                       stat = remap_field_dryrun(nIsobaricLev,nOznLevels,nSoilLevels,remap_info, field, target_field)
                    else if(TRIM(verticalCoord) == 'MPAS_Model')then
                       stat = remap_field_dryrun(nVertLevels,nOznLevels,nSoilLevels,remap_info, field, target_field)
                    else
                       write(0,*) 'error at verticalCoord=',TRIM(verticalCoord)
                       stop 44
                    endif

                    stat = file_output_register_field(output_handle, target_field)
                    stat = copy_field_atts(handle, field, output_handle, target_field)
                    if (stat /= 0) then
                        stat = free_target_field(target_field)
                        stat = scan_input_free_field(field)
                        stat = scan_input_close(handle)
                        stat = file_output_close(output_handle)
                        stat = mpas_mesh_free(source_mesh)
                        stat = target_mesh_free(destination_mesh)
                        stat = remap_info_free(remap_info)
                        stat = field_list_finalize(include_field_list, exclude_field_list)
                        stop 7
                    end if
        
                    stat = free_target_field(target_field)
                end if
                stat = scan_input_free_field(field)
            end do

            ! 
            ! Write 'lat' and 'lon' fields for target mesh
            ! 
            stat = remap_get_target_latitudes(remap_info, target_field)
            stat = file_output_write_field(output_handle, target_field, frame=0)
            stat = free_target_field(target_field)

            stat = remap_get_target_longitudes(remap_info, target_field)
            stat = file_output_write_field(output_handle, target_field, frame=0)
            stat = free_target_field(target_field)

            if(TRIM(verticalCoord) == 'Pressure')then
               stat = remap_get_target_t_iso_levels(remap_info, target_field,nIsobaricLev)
               stat = file_output_register_field(output_handle, target_field)
               stat = free_target_field(target_field)

               stat = remap_get_target_u_iso_levels(remap_info, target_field,nIsobaricLev)
               stat = file_output_register_field(output_handle, target_field)
               stat = free_target_field(target_field)

            else if(TRIM(verticalCoord) == 'MPAS_Model')then   
               stat = remap_get_target_nVertLevels(remap_info, target_field,nVertLevels)
               stat = file_output_register_field(output_handle, target_field)
               stat = free_target_field(target_field)

            else
               write(0,*) 'error at verticalCoord=',TRIM(verticalCoord)
               stop 66
            endif

            stat = remap_get_target_time(remap_info, target_field)
            stat = file_output_register_field(output_handle, target_field)
            stat = free_target_field(target_field)

            ! Add units, long_name, standard_name attribute to coordinate variables.
            stat = add_latlon_atts(output_handle)


            if(TRIM(verticalCoord) == 'Pressure')then
	       stat = add_zlevels_atts(output_handle)
            else if(TRIM(verticalCoord) == 'MPAS_Model')then   
	       stat = add_zlevels_model_atts(output_handle)
            else
               write(0,*) 'error at verticalCoord=',TRIM(verticalCoord)
               stop 66
            endif
            stat = add_time_atts(output_handle,Time_Label)
        end if


        !
        ! Loop over all times in the input file
        !
        do iRec=1,nRecordsIn
            stat = scan_input_rewind(handle)
    
            !
            ! Scan through list of fields in the input file, remapping fields and writing
            ! them to the output file
            !
            if(TRIM(verticalCoord) == 'MPAS_Model')then   
        
	            call timer_start(remap_timer)
                    stat = remap_field1DM(nVertLevels,remap_info, target_field)
                    call timer_stop(remap_timer)
                    write(0,'(a,f10.6,a)') '    remap: ', timer_time(remap_timer), ' s'
 
                    call timer_start(write_timer)
                    stat = file_output_write_field(output_handle, target_field, frame=(nRecordsOut+iRec))
                    call timer_stop(write_timer)
                    write(0,'(a,f10.6,a)') '    write: ', timer_time(write_timer), ' s'
                    stat = free_target_field(target_field)

            endif

            do while (scan_input_next_field(handle, field) == 0) 
                if (can_remap_field(field) .and. &
                    should_remap_field(field, include_field_list, exclude_field_list)) then
                    write(0,*) 'Remapping field '//trim(field % name)//', frame ', irec
    
                    call timer_start(read_timer)
                    stat = scan_input_read_field(field, frame=iRec)
                    call timer_stop(read_timer)
                    write(0,'(a,f10.6,a)') '    read: ', timer_time(read_timer), ' s'
    
                    call timer_start(remap_timer)
                    stat = remap_field(remap_info, field, target_field)
                    call timer_stop(remap_timer)
                    write(0,'(a,f10.6,a)') '    remap: ', timer_time(remap_timer), ' s'
    
                    call timer_start(write_timer)
                    stat = file_output_write_field(output_handle, target_field, frame=(nRecordsOut+iRec))
                    call timer_stop(write_timer)
                    write(0,'(a,f10.6,a)') '    write: ', timer_time(write_timer), ' s'
    
                    stat = free_target_field(target_field)
                else if ((trim(field % name) == 't_iso_levels' .or. trim(field % name) == 'z_iso_levels' &
		    .or. trim(field % name) == 'u_iso_levels') .and. TRIM(verticalCoord) == 'Pressure')then
                    write(0,*) 'No remapping field '//trim(field % name)//', frame ', irec
    
                    call timer_start(read_timer)
                    stat = scan_input_read_field(field, frame=iRec)
                    call timer_stop(read_timer)
                    write(0,'(a,f10.6,a)') '    read: ', timer_time(read_timer), ' s'
        
	            call timer_start(remap_timer)
                    stat = remap_field1D(verticalCoord,remap_info, field, target_field)
                    call timer_stop(remap_timer)
                    write(0,'(a,f10.6,a)') '    remap: ', timer_time(remap_timer), ' s'
 
                    call timer_start(write_timer)
                    stat = file_output_write_field(output_handle, target_field, frame=(nRecordsOut+iRec))
                    call timer_stop(write_timer)
                    write(0,'(a,f10.6,a)') '    write: ', timer_time(write_timer), ' s'
                    stat = free_target_field(target_field)


                else if (trim(field % name) == 'xtime' )then
                    write(0,*) 'No remapping field '//trim(field % name)//', frame ', irec
    
                    call timer_start(read_timer)
                    stat = scan_input_read_field(field, frame=iRec)
                    call timer_stop(read_timer)
                    write(0,'(a,f10.6,a)') '    read: ', timer_time(read_timer), ' s'
        
	            call timer_start(remap_timer)
                    stat = remap_fieldTime(remap_info, field, target_field)
                    call timer_stop(remap_timer)
                    write(0,'(a,f10.6,a)') '    remap: ', timer_time(remap_timer), ' s'
 
                    call timer_start(write_timer)
                    stat = file_output_write_field(output_handle, target_field, frame=(nRecordsOut+iRec))
                    call timer_stop(write_timer)
                    write(0,'(a,f10.6,a)') '    write: ', timer_time(write_timer), ' s'
                    stat = free_target_field(target_field)

                end if
                stat = scan_input_free_field(field)
            end do
        end do
    
        nRecordsOut = nRecordsOut + nRecordsIn
        stat = scan_input_close(handle)
    end do


    !
    ! Cleanup
    !
    stat = file_output_close(output_handle)

    stat = mpas_mesh_free(source_mesh)
    stat = target_mesh_free(destination_mesh)
    stat = remap_info_free(remap_info)
    stat = field_list_finalize(include_field_list, exclude_field_list)

    call timer_stop(total_timer)

    write(0,*) ' '
    write(0,'(a,f10.6)') 'Total runtime: ', timer_time(total_timer)
    write(0,*) ' '

    stop
contains
 function get_filename(full_filename) result(filename)
    implicit none
    character(len=*), INTENT(in) :: full_filename
    character(len=1000) :: path, directory, extension
    character(len=1000) :: filename

    integer :: i
    ! Encontre a última barra no caminho
    directory = ''    
    do i = len(full_filename), 1, -1
        if (full_filename(i:i) .eq. '/') then
            directory = full_filename(1:i)
            filename  = ''
	    exit
        else 
            directory = './'
	    filename  = trim(full_filename)
        endif
    enddo
    if( trim(filename) == trim(full_filename))return

    ! Se não encontrou a barra, o diretório é vazio
    if (directory .eq. '') then
        directory = './'
    endif

    ! Extrair o nome do arquivo e a extensão
    extension = ''
    do i = len(full_filename), 1, -1
        if (full_filename(i:i) .eq. '.') then
            extension = full_filename(i+1:)
            exit
        endif
    enddo
    ! Extrair o nome do arquivo e a extensão
    filename = ''
    do i = len(full_filename), 1, -1
        if (full_filename(i:i) .eq. '/') then
            filename  = full_filename(i+1:)
            exit
        endif
    enddo


end function get_filename
end program convert_mpas
