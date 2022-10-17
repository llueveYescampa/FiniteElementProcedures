# Include the directory itself as a path to include directories
enable_language(Fortran)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
get_filename_component(CurrDir ${CMAKE_CURRENT_SOURCE_DIR} NAME)
  
#Create a variable called linalg_SOURCES containing all .cpp files:

set(colsol_Src 
    ${myPROJECT_SOURCE_DIR}/${CurrDir}/colsol.f90
    PARENT_SCOPE
    )

set(colsol_Inc
    ${myPROJECT_SOURCE_DIR}/colsol/include
    PARENT_SCOPE
    )
