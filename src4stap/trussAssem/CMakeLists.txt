# Include the directory itself as a path to include directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
get_filename_component(CurrDir ${CMAKE_CURRENT_SOURCE_DIR} NAME)

#Create a variable called linalg_SOURCES containing all .f90 files:

set(trussAssem_Src
    ${myPROJECT_SOURCE_DIR}/${CurrDir}/trussAssem.f90
    PARENT_SCOPE
    )
