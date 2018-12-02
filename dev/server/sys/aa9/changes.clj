(ns sys.aa9.changes
  (:require [query :as q]
            [sys.aa9.controller :as aa9]))

(def fake-changes
  [{:routine-name      "Initialise"
    :rung-num          10
    :expected-rung     [:rung
                        ["XIO" ["TEMP_Failed_Initialisation"]] ["MOV" ["1" "STATE"]]]
    :replace-with-rung [:rung]}])

(def changes {
   aa9/main-program
   [{:routine-name      "HMI_Read"
     :expected-rung     [:rung ["COP" ["Local:7:I.Ch0Data" "HMI_R_PLC.S7" "16"]]]
     :replace-with-rung [:rung ["COP" ["Local:7:I.Ch0Data" "HMI_R_PLC.S7.AI00" "16"]]]}
    {:routine-name      "Initialise"
     :expected-rung     [:rung ["JSR" ["Init_Sequence_Buffer" "0"]]]
     :replace-with-rung [:rung ["AFI" [""]] ["JSR" ["Init_Sequence_Buffer" "0"]]]}]
   aa9/common-program
   [
    #_{:routine-name      "Control_Logic"
       :expected-rung     [:rung ["FLL" ["0" "Null_Char" "1"]]]
       :replace-with-rung [:rung ["FLL" ["0" "Null_Char.LEN" "1"]]]}
    #_{:routine-name      "Control_Logic"
       :expected-rung     [:rung ["FLL" ["0" "Null_Char" "1"]]]
       :replace-with-rung [:rung ["MOV" ["0" "Null_Char"]]]}
    ;;
    ;; Don't want what we are feeding in being overwritten by rubbish from Modbus
    ;;
    {:routine-name      "CONV_REAL_A2"
     :expected-rung     [:rung ["MUL" ["A2_CO_RAW" "1.01522" "A2_CARBON_MONOXIDE"]]]
     :replace-with-rung [:rung ["NOP" [""]]]}
    {:routine-name      "CONV_REAL_A1"
     :expected-rung     [:rung ["MUL" ["A1_METHANE_RAW" "1" "A1_METHANE"]]]
     :replace-with-rung [:rung ["NOP" [""]]]}
    {:routine-name      "CONV_REAL_A2"
     :expected-rung     [:rung ["MUL" ["A2_OXYGEN_RAW" "1" "A2_OXYGEN"]]]
     :replace-with-rung [:rung ["NOP" [""]]]}
    {:routine-name      "CONV_REAL_A1"
     :expected-rung     [:rung ["MUL" ["A1_CO2_RAW" "1.0256" "A1_CARBON_DIOXIDE"]]]
     :replace-with-rung [:rung ["NOP" [""]]]}]

   aa9/modbus-program
   [{:routine-name      "ClearTransTrigger"
     :expected-rung     [:rung
                         ["EQU" ["CIdx" "0"]] ["OTU" ["MBTU_Transactions_00[Index].ReqBuilt"]] ["OTU" ["Trans_Triggers_00[Index]"]]]
     :replace-with-rung [:rung
                         ["EQU" ["CIdx" "0"]] [:br
                                               [:br-lev ["OTU" ["MBTU_Transactions_00[Index].ReqBuilt"]]]
                                               [:br-lev ["OTU" ["Trans_Triggers_00[Index]"]]]
                                               ]]}
    {:routine-name      "ClearTransTrigger"
     :expected-rung     [:rung
                         ["EQU" ["CIdx" "1"]] ["OTU" ["MBTU_Transactions_01[Index].ReqBuilt"]] ["OTU" ["Trans_Triggers_01[Index]"]]]
     :replace-with-rung [:rung
                         ["EQU" ["CIdx" "1"]] [:br
                                               [:br-lev ["OTU" ["MBTU_Transactions_01[Index].ReqBuilt"]]]
                                               [:br-lev ["OTU" ["Trans_Triggers_01[Index]"]]]
                                               ]]}
    {:routine-name      "ClearTransTrigger"
     :expected-rung     [:rung
                         ["EQU" ["CIdx" "2"]] ["OTU" ["MBTU_Transactions_02[Index].ReqBuilt"]] ["OTU" ["Trans_Triggers_02[Index]"]]]
     :replace-with-rung [:rung
                         ["EQU" ["CIdx" "2"]] [:br
                                               [:br-lev ["OTU" ["MBTU_Transactions_02[Index].ReqBuilt"]]]
                                               [:br-lev ["OTU" ["Trans_Triggers_02[Index]"]]]
                                               ]]}
    {:routine-name      "ClearTransTrigger"
     :expected-rung     [:rung
                         ["EQU" ["CIdx" "3"]] ["OTU" ["MBTU_Transactions_03[Index].ReqBuilt"]] ["OTU" ["Trans_Triggers_03[Index]"]]]
     :replace-with-rung [:rung
                         ["EQU" ["CIdx" "3"]] [:br
                                               [:br-lev ["OTU" ["MBTU_Transactions_03[Index].ReqBuilt"]]]
                                               [:br-lev ["OTU" ["Trans_Triggers_03[Index]"]]]
                                               ]]}]

   })
