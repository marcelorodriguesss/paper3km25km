Module gdf_vars

type var_tab
   character(len=32) revu_name,gdf_name,units,cformat
end type

integer, parameter :: max_gvars=100
type(var_tab) :: gvar_table(max_gvars)

integer :: num_gvars

Contains

!---------------------------------------------------------------------------

subroutine gtab(rname,gname,unit,fmt)
implicit none
character(len=*) :: rname,gname,unit,fmt

num_gvars=num_gvars+1
if (num_gvars > max_gvars) then
   print*,'gdf_vars: exceeded max_gvars'
   stop 'gdf_vars-1'
endif

gvar_table(num_gvars)%revu_name=rname
gvar_table(num_gvars)%gdf_name=gname
gvar_table(num_gvars)%units=unit
gvar_table(num_gvars)%cformat=fmt

return
end subroutine

!***************************************************************************

subroutine fill_var_tab ()

implicit none

! Define the output variable names, units, and format. Format includes
!    field for QC flags


call gtab('ue',         'WIND_U_COMPONENT',  'm/s',   '2x,f9.2,2x,i4.3')
call gtab('ve',         'WIND_V_COMPONENT',  'm/s',   '2x,f9.2,2x,i4.3')
call gtab('ue_avg',     'WIND_U_COMPONENT',  'm/s',   '2x,f9.2,2x,i4.3')
call gtab('ve_avg',     'WIND_V_COMPONENT',  'm/s',   '2x,f9.2,2x,i4.3')
call gtab('speed',      'WINDSPEED',         'm/s',   '2x,f9.2,2x,i4.3')
call gtab('direction',  'WIND_DIRECTION',    'deg',   '2x,f7.0,2x,i4.3')
call gtab('tempc',      'TEMPERATURE',       'C',     '2x,f7.1,2x,i4.3')
call gtab('tempf',      'TEMPERATURE',       'F',     '2x,f7.1,2x,i4.3')
call gtab('dewptc',     'DEWPOINT',          'C',     '2x,f7.1,2x,i4.3')
call gtab('dewptf',     'DEWPOINT',          'F',     '2x,f7.1,2x,i4.3')
call gtab('dewptk',     'DEWPOINT',          'K',     '2x,f7.1,2x,i4.3')
call gtab('relhum',     'RELATIVE_HUMIDITY', 'pct',   '2x,f7.0,2x,i4.3')
call gtab('press',      'STN_PRES',          'mb',    '2x,f10.3,2x,i4.3')
call gtab('press_pa',   'STN_PRES',          'Pa',    '2x,f10.1,2x,i4.3')
call gtab('sea_press',  'SLP',               'Pa',    '2x,f10.1,2x,i4.3')
call gtab('cloud_frac', 'CLOUD_COVER',       'fraction', '2x,f6.3,2x,i4.3')
call gtab('ch-o3-ug',   'O3',                'ug/m3',    '2x,f10.3,2x,i4.3')
call gtab('ch-no-ug',   'NO',                'ug/m3',    '2x,f10.3,2x,i4.3')
call gtab('ch-no2-ug',  'NO2',               'ug/m3',    '2x,f10.3,2x,i4.3')
call gtab('ch-so2-ug',  'SO2',               'ug/m3',    '2x,f10.3,2x,i4.3')
call gtab('ch-voc-ug',  'VOC',               'ug/m3',    '2x,f10.3,2x,i4.3')
call gtab('ch-pm10-ug', 'PM10',              'ug/m3',    '2x,f10.3,2x,i4.3')
call gtab('em-no',      'EM-NO',             'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-no2',     'EM-NO2',            'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-par',     'EM-PAR',            'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-tol',     'EM-TOL',            'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-eth',     'EM-ETH',            'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-ole',     'EM-OLE',            'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-isop',    'EM-ISOP',           'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-xyl',     'EM-XYL',            'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-form',    'EM-FORM',           'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-ald2',    'EM-ALD2',           'moles/hr', '2x,f10.4,2x,i4.3')
call gtab('em-so2',     'EM-SO2',            'moles/hr', '2x,f10.4,2x,i4.3')

return
end subroutine

!***************************************************************************

subroutine ralph_vars (ivok,nvar,cvar,nvr,namev,unitv,cfmt)

implicit none
integer :: ivok,nvar,nvr
character(len=*) :: cvar,namev,unitv,cfmt
logical, save :: first_call=.true.
integer :: it

if (first_call) then
   first_call = .false.
   call fill_var_tab()
endif

! Search the table for variable info
print*,'+++++++:', trim(cvar), num_gvars
do it = 1,num_gvars
   if (trim(cvar) == trim(gvar_table(it)%revu_name)) then
      namev = gvar_table(it)%gdf_name
      unitv = gvar_table(it)%units
      cfmt = gvar_table(it)%cformat
      nvr=nvar
      ivok=1
      return
   endif
enddo      


print*,'ralph_vars: variable not in GDF file variable list: ',trim(cvar)
stop 'ralph_vars: variable not in GDF file variable list'

return
end subroutine

end Module
