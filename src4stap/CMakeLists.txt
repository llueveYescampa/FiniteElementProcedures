# Include the directory itself as a path to include directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

#Create a variable called linalg_SOURCES containing all .cpp files:


add_subdirectory(addban)
add_subdirectory(addres)
add_subdirectory(assem)
add_subdirectory(clear)
add_subdirectory(colht)
add_subdirectory(colsol)
add_subdirectory(elcal)
add_subdirectory(elemnt)
add_subdirectory(elemntAssem)
add_subdirectory(elemntStress)
#add_subdirectory(error)
add_subdirectory(input)
add_subdirectory(loads)
add_subdirectory(loadv)
add_subdirectory(russ)
#add_subdirectory(russ2)
#add_subdirectory(russ3)
add_subdirectory(getSeconds)
add_subdirectory(stap)
add_subdirectory(stress)
add_subdirectory(truss)
add_subdirectory(trussAssem)
add_subdirectory(trussStress)
add_subdirectory(writed)

set(stap_Src 
    ${addban_Src}
    ${addres_Src}
    ${assem_Src}
    ${clear_Src}
    ${colht_Src}
    ${colsol_Src}
    ${elcal_Src}
    ${elemnt_Src}
    ${elemntAssem_Src}
    ${elemntStress_Src}
#    ${error_Src}
    ${input_Src}
    ${loads_Src}
    ${loadv_Src}
    ${russ_Src}
    ${getSeconds_Src}
    ${stap_Src}
    ${stress_Src}
    ${truss_Src}
    ${trussAssem_Src}
    ${trussStress_Src}
    ${writed_Src}
    PARENT_SCOPE
    )

set(stap_Inc
    ${myPROJECT_SOURCE_DIR}/include
    ${colsol_Inc}
    ${russ_Inc}
    ${input_Inc}
    ${stap_Inc}
    PARENT_SCOPE
    )


