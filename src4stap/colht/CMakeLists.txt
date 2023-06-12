# Include the directory itself as a path to include directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
get_filename_component(CurrDir ${CMAKE_CURRENT_SOURCE_DIR} NAME)

#Create a variable called linalg_SOURCES containing all .f95 files:

set(colht_Src
    ${myPROJECT_SOURCE_DIR}/${CurrDir}/colht.f95
    PARENT_SCOPE
    )
