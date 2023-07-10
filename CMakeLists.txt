cmake_minimum_required(VERSION 3.16)

## set appropriately the following line to create a Double Precision version
#set(DOUBLE_PRECISION ON)


project(stap Fortran)
enable_language(Fortran)

# adjust following line for your project
set(myPROJECT_SOURCE_DIR ${PROJECT_SOURCE_DIR}/src4stap)

set(CMAKE_BUILD_TYPE Release)
#set(CMAKE_BUILD_TYPE Debug)

## adding the following lines while cleaning the code
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    MESSAGE( STATUS "Fortran_COMPILER_NAME: "            ${CMAKE_Fortran_COMPILER_ID} )
    set(CMAKE_Fortran_FLAGS    " -Wall -Wextra -Wno-unused-parameter -ftree-vectorize -fopt-info-vec-optimized -ffast-math -mavx2")
    #set(CMAKE_Fortran_FLAGS    " -Wall -ffree-form  -fimplicit-none  -Wextra -Wno-unused-parameter -ftree-vectorize -fopt-info-vec-optimized -ffast-math -mavx2")
elseif ( CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" )
    MESSAGE( STATUS "Fortran_COMPILER_ID: "            ${CMAKE_Fortran_COMPILER_ID} )
    set(CMAKE_Fortran_FLAGS  "   -fvectorize  -qopt-report-file=stderr -qopt-report=max -xCORE-AVX2")
    #set(CMAKE_Fortran_FLAGS  " -Wall -qopt-report=3 -qopt-report-phase=vec ")
elseif ( CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC" )
    MESSAGE( STATUS "Fortran_COMPILER_NAME: "            ${CMAKE_Fortran_COMPILER_ID} )
    set(CMAKE_Fortran_FLAGS  " -Wall -Minform=inform -Minfo=all -Mvect=simd ")
    #set(CMAKE_Fortran_FLAGS  " -Minform=inform   -Minfo=all -Mvect=simd -tp=px ")
    # DO NOT USE THE "-Mnodepchk" flag
endif ( )
## end of adding the following lines while cleaning the code

if (DOUBLE_PRECISION)
    set(CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -DDOUBLE")
endif()



add_subdirectory(${myPROJECT_SOURCE_DIR})
include_directories( ${stap_Inc})
add_executable(stap ${stap_Src})
target_link_libraries(stap  ${CMAKE_EXE_LINKER_FLAGS})


############### Debug messages ####################
#MESSAGE( STATUS "Fortran_COMPILER_NAME: "            ${CMAKE_Fortran_COMPILER_ID} )
#MESSAGE( STATUS "Fortran_COMPILER_NAME: "            ${CMAKE_Fortran_COMPILER} )
#MESSAGE( STATUS "                        myPROJECT_SOURCE_DIR:  "            ${myPROJECT_SOURCE_DIR} )
#MESSAGE( STATUS "CMAKE_BINARY_DIR: "            ${CMAKE_BINARY_DIR} )
#MESSAGE( STATUS "CMAKE_C_COMPILER_ID: "         ${CMAKE_C_COMPILER_ID} )
#MESSAGE( STATUS "CMAKE_C_FLAGS: "               ${CMAKE_C_FLAGS} )
#MESSAGE( STATUS "CMAKE_CURRENT_SOURCE_DIR: "    ${CMAKE_CURRENT_SOURCE_DIR} )
########### end of Debug messages #################
