# Include the directory itself as a path to include directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
get_filename_component(CurrDir ${CMAKE_CURRENT_SOURCE_DIR} NAME)

#Create a variable called linalg_SOURCES containing all .f95 files:

set(russ_Src
    ${myPROJECT_SOURCE_DIR}/${CurrDir}/russ1.f95
    ${myPROJECT_SOURCE_DIR}/${CurrDir}/russ2.f95
    ${myPROJECT_SOURCE_DIR}/${CurrDir}/russ3.f95
    PARENT_SCOPE
    )

set(russ_Inc
    ${myPROJECT_SOURCE_DIR}/russ/include
    PARENT_SCOPE
    )
