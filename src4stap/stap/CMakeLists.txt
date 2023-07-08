# Include the directory itself as a path to include directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
get_filename_component(CurrDir ${CMAKE_CURRENT_SOURCE_DIR} NAME)

#Create a variable called linalg_SOURCES containing all .f90 files:

set(stap_Src
    ${myPROJECT_SOURCE_DIR}/${CurrDir}/stap.f90
    PARENT_SCOPE
    )

set(stap_Inc
    ${myPROJECT_SOURCE_DIR}/stap/include
    PARENT_SCOPE
    )
