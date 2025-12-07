; ==========================================================
; Hospital Management System (Array DB) - MASM
; ==========================================================

INCLUDE Irvine32.inc

MAX_PATIENTS = 100
MAX_NAME_LEN = 50
MAX_BEDS     = 20

.data
; -------------------------
; UI strings
; -------------------------
msgBanner1      BYTE "==================================",0
msgBanner2      BYTE "   Hospital Management (MASM)    ",0
msgPressAny     BYTE "Press any key to continue...",0
msgBedLabel     BYTE "Bed ",0
msgColon        BYTE ": ",0
msgOccupied     BYTE "Occupied",0

usernameMsg     BYTE "Enter username (a = admin, u = user): ",0

menuText        BYTE 0Dh,0Ah,
                 "1. Add Patient",0Dh,0Ah,
                 "2. View Patients",0Dh,0Ah,
                 "3. View All Beds",0Dh,0Ah,
                 "4. Generate Bill",0Dh,0Ah,
                 "5. Remove Patient",0Dh,0Ah,
                 "6. Exit",0Dh,0Ah,
                 "Enter choice: ",0

promptName      BYTE "Enter patient name: ",0
promptAge       BYTE "Enter patient age: ",0
promptDays      BYTE "Enter days stayed: ",0
promptService   BYTE "Enter service charges: ",0
promptDocFee    BYTE "Enter doctor fee: ",0
promptRoomRate  BYTE "Enter room rate per day: ",0
RoomRateTemp    DWORD ?

msgAdded        BYTE "Patient added successfully!",0
msgFull         BYTE "Patient list is full!",0
msgBedsFull     BYTE "No beds available!",0
msgNoPatients   BYTE "No patient records found.",0
msgRemoved      BYTE "Patient removed.",0
msgNotFound     BYTE "Patient not found.",0

msgName         BYTE "Name: ",0
msgAge          BYTE "Age: ",0
msgBed          BYTE "Bed: ",0
msgDays         BYTE "Days: ",0
msgService      BYTE "Service: ",0
msgDocFee       BYTE "Doctor Fee: ",0
msgTotalBill    BYTE "Total Bill: ",0

msgBedsTitle    BYTE "--- BED STATUS ---",0
msgBedFree      BYTE "Free",0

newline         BYTE 0Dh,0Ah,0

; -------------------------
; Temporary buffers
; -------------------------
tempName        BYTE MAX_NAME_LEN DUP(0)
tempBuffer      BYTE MAX_NAME_LEN DUP(0)

; -------------------------
; Database (parallel arrays)
; -------------------------
patientNames    BYTE MAX_PATIENTS * MAX_NAME_LEN DUP(0)
patientAges     DWORD MAX_PATIENTS DUP(0)
patientBeds     DWORD MAX_PATIENTS DUP(0)
patientDays     DWORD MAX_PATIENTS DUP(0)
patientService  DWORD MAX_PATIENTS DUP(0)
patientDocFee   DWORD MAX_PATIENTS DUP(0)
patientTotal    DWORD MAX_PATIENTS DUP(0)

totalPatients   DWORD 0

; -------------------------
; Beds: 1 = free, 0 = occupied
; -------------------------
bedStatus       DWORD MAX_BEDS DUP(1)
bedOwner        DWORD MAX_BEDS DUP(0) ; stores patient index + 1 or 0 if free

; -------------------------
; Temporaries for numeric inputs
; -------------------------
AgeTemp         DWORD ?
DaysTemp        DWORD ?
ServiceTemp     DWORD ?
DocFeeTemp      DWORD ?

.code

; ==========================================================
; Display banner
; ==========================================================
DisplayBanner PROC
    mov edx, OFFSET msgBanner1
    call WriteString
    call Crlf
    mov edx, OFFSET msgBanner2
    call WriteString
    call Crlf
    mov edx, OFFSET msgBanner1
    call WriteString
    call Crlf
    ret
DisplayBanner ENDP

; ==========================================================
; AssignBed -> eax = bed number (1..MAX_BEDS) or 0 if none
; ==========================================================
AssignBed PROC
    push esi
    mov esi, OFFSET bedStatus
    mov ecx, MAX_BEDS
    xor eax, eax
    mov ebx, 0
ab_loop:
    mov edx, [esi]        ; bedStatus[ebx]
    cmp edx, 1
    jne ab_next
    ; found free bed
    mov dword ptr [esi], 0 ; mark occupied
    mov eax, ebx
    inc eax                ; bed number = index + 1
    pop esi
    ret
ab_next:
    add esi, 4
    inc ebx
    dec ecx
    jnz ab_loop
    xor eax, eax
    pop esi
    ret
AssignBed ENDP

; ==========================================================
; FreeBed: eax = bed number (1..MAX_BEDS)
; ==========================================================
FreeBed PROC
    cmp eax, 1
    jl fb_ret
    cmp eax, MAX_BEDS
    jg fb_ret
    mov ebx, eax
    dec ebx
    mov esi, OFFSET bedStatus
    mov dword ptr [esi + ebx*4], 1
    mov esi, OFFSET bedOwner
    mov dword ptr [esi + ebx*4], 0
fb_ret:
    ret
FreeBed ENDP

; ==========================================================
; AddPatient - now includes variable room rates
; ==========================================================
AddPatient PROC
    pushad

    mov eax, totalPatients
    cmp eax, MAX_PATIENTS
    jae AP_FULL

    ; Read name
    mov edx, OFFSET promptName
    call WriteString
    mov edx, OFFSET tempName
    mov ecx, MAX_NAME_LEN
    call ReadString

    ; Read age
    mov edx, OFFSET promptAge
    call WriteString
    call ReadInt
    mov [AgeTemp], eax

    ; Read days
    mov edx, OFFSET promptDays
    call WriteString
    call ReadInt
    mov [DaysTemp], eax

    ; Read service
    mov edx, OFFSET promptService
    call WriteString
    call ReadInt
    mov [ServiceTemp], eax

    ; Read doctor fee
    mov edx, OFFSET promptDocFee
    call WriteString
    call ReadInt
    mov [DocFeeTemp], eax

    ; Read room rate
    mov edx, OFFSET promptRoomRate
    call WriteString
    call ReadInt
    mov [RoomRateTemp], eax

    ; Assign bed
    call AssignBed
    cmp eax, 0
    je AP_NOBED
    mov ebx, eax        ; bed number

    ; record index = totalPatients
    mov esi, totalPatients

    ; store name
    mov eax, esi
    imul eax, MAX_NAME_LEN
    lea edi, patientNames
    add edi, eax
    lea esi, tempName
    mov ecx, MAX_NAME_LEN
    cld
    rep movsb

    ; store ages
    mov eax, totalPatients
    mov edx, [AgeTemp]
    mov [patientAges + eax*4], edx

    ; store bed
    mov eax, totalPatients
    mov [patientBeds + eax*4], ebx

    ; store days
    mov eax, totalPatients
    mov edx, [DaysTemp]
    mov [patientDays + eax*4], edx

    ; store service
    mov eax, totalPatients
    mov edx, [ServiceTemp]
    mov [patientService + eax*4], edx

    ; store doctor fee
    mov eax, totalPatients
    mov edx, [DocFeeTemp]
    mov [patientDocFee + eax*4], edx

    ; compute total = days * roomRate + service + docFee
    mov eax, [DaysTemp]
    imul eax, [RoomRateTemp]   ; multiply by variable room rate
    add eax, [ServiceTemp]
    add eax, [DocFeeTemp]
    mov edx, totalPatients
    mov [patientTotal + edx*4], eax

    ; map bed -> owner (patientIndex + 1)
    mov eax, ebx
    dec eax
    mov ecx, totalPatients
    inc ecx
    mov esi, OFFSET bedOwner
    mov [esi + eax*4], ecx

    ; increment totalPatients
    mov eax, totalPatients
    inc eax
    mov totalPatients, eax

    mov edx, OFFSET msgAdded
    call WriteString
    call Crlf
    jmp AP_END

AP_NOBED:
    mov edx, OFFSET msgBedsFull
    call WriteString
    call Crlf
    jmp AP_END

AP_FULL:
    mov edx, OFFSET msgFull
    call WriteString
    call Crlf

AP_END:
    popad
    ret
AddPatient ENDP

; ==========================================================
; ViewPatients
; ==========================================================
ViewPatients PROC
    pushad
    mov eax, totalPatients
    cmp eax, 0
    je VP_NONE

    xor ecx, ecx
VP_LOOP:
    mov edx, OFFSET newline
    call WriteString

    ; Name
    mov edx, OFFSET msgName
    call WriteString
    mov eax, ecx
    imul eax, MAX_NAME_LEN
    lea ebx, patientNames
    add ebx, eax
    mov edx, ebx
    call WriteString
    call Crlf

    ; Age
    mov edx, OFFSET msgAge
    call WriteString
    mov eax, [patientAges + ecx*4]
    call WriteDec
    call Crlf

    ; Bed
    mov edx, OFFSET msgBed
    call WriteString
    mov eax, [patientBeds + ecx*4]
    call WriteDec
    call Crlf

    ; Days
    mov edx, OFFSET msgDays
    call WriteString
    mov eax, [patientDays + ecx*4]
    call WriteDec
    call Crlf

    ; Service
    mov edx, OFFSET msgService
    call WriteString
    mov eax, [patientService + ecx*4]
    call WriteDec
    call Crlf

    ; Doctor Fee
    mov edx, OFFSET msgDocFee
    call WriteString
    mov eax, [patientDocFee + ecx*4]
    call WriteDec
    call Crlf

    ; Total
    mov edx, OFFSET msgTotalBill
    call WriteString
    mov eax, [patientTotal + ecx*4]
    call WriteDec
    call Crlf

    inc ecx
    mov eax, totalPatients
    cmp ecx, eax
    jl VP_LOOP
    jmp VP_DONE

VP_NONE:
    mov edx, OFFSET msgNoPatients
    call WriteString
    call Crlf

VP_DONE:
    mov edx, OFFSET msgPressAny
    call WriteString
    call ReadChar
    popad
    ret
ViewPatients ENDP

; ==========================================================
; ViewAllBeds
; ==========================================================
ViewAllBeds PROC
    pushad
    mov edx, OFFSET msgBedsTitle
    call WriteString
    call Crlf

    xor ecx, ecx
BED_LOOP:
    mov edx, OFFSET msgBedLabel
    call WriteString
    mov eax, ecx
    inc eax
    call WriteDec
    mov edx, OFFSET msgColon
    call WriteString

    mov esi, OFFSET bedStatus
    mov ebx, [esi + ecx*4]
    cmp ebx, 1
    je BED_FREE

    ; occupied
    mov esi, OFFSET bedOwner
    mov eax, [esi + ecx*4]
    cmp eax, 0
    je BED_UNKNOWN
    dec eax
    imul eax, MAX_NAME_LEN
    lea ebx, patientNames
    add ebx, eax
    mov edx, ebx
    call WriteString
    jmp BED_NEXT

BED_UNKNOWN:
    mov edx, OFFSET msgOccupied
    call WriteString
    jmp BED_NEXT

BED_FREE:
    mov edx, OFFSET msgBedFree
    call WriteString

BED_NEXT:
    call Crlf
    inc ecx
    cmp ecx, MAX_BEDS
    jl BED_LOOP

    mov edx, OFFSET msgPressAny
    call WriteString
    call ReadChar
    popad
    ret
ViewAllBeds ENDP

; ==========================================================
; GenerateBill
; ==========================================================
GenerateBill PROC
    pushad
    mov edx, OFFSET promptName
    call WriteString
    mov edx, OFFSET tempName
    mov ecx, MAX_NAME_LEN
    call ReadString

    xor ecx, ecx
GB_LOOP:
    mov eax, totalPatients
    cmp ecx, eax
    jge GB_NOTFOUND

    ; compare names
    mov ebx, ecx
    imul ebx, MAX_NAME_LEN
    lea esi, patientNames
    add esi, ebx
    lea edi, tempName
    mov edx, MAX_NAME_LEN

GB_CMP:
    mov al, [esi]
    mov bl, [edi]
    cmp al, bl
    jne GB_NEXT
    cmp al, 0
    je GB_MATCH
    inc esi
    inc edi
    dec edx
    jnz GB_CMP
    jmp GB_MATCH

GB_NEXT:
    inc ecx
    jmp GB_LOOP

GB_MATCH:
    mov edx, OFFSET msgTotalBill
    call WriteString
    mov eax, [patientTotal + ecx*4]
    call WriteDec
    call Crlf
    jmp GB_DONE

GB_NOTFOUND:
    mov edx, OFFSET msgNotFound
    call WriteString
    call Crlf

GB_DONE:
    mov edx, OFFSET msgPressAny
    call WriteString
    call ReadChar
    popad
    ret
GenerateBill ENDP

; ==========================================================
; RemovePatient
; ==========================================================
RemovePatient PROC
    pushad

    mov edx, OFFSET promptName
    call WriteString
    mov edx, OFFSET tempName
    mov ecx, MAX_NAME_LEN
    call ReadString

    xor ecx, ecx
RN_SEARCH_LOOP:
    mov eax, totalPatients
    cmp ecx, eax
    jge RN_NOTFOUND

    mov ebx, ecx
    imul ebx, MAX_NAME_LEN
    lea esi, patientNames
    add esi, ebx
    lea edi, tempName
    mov edx, MAX_NAME_LEN

RN_CMP_LOOP:
    mov al, [esi]
    mov bl, [edi]
    cmp al, bl
    jne RN_NEXT_IDX
    cmp al, 0
    je RN_FOUND
    inc esi
    inc edi
    dec edx
    jnz RN_CMP_LOOP
    jmp RN_FOUND

RN_NEXT_IDX:
    inc ecx
    jmp RN_SEARCH_LOOP

RN_NOTFOUND:
    mov edx, OFFSET msgNotFound
    call WriteString
    call Crlf
    jmp RN_DONE_POP

RN_FOUND:
    ; free bed if assigned
    mov eax, [patientBeds + ecx*4]
    cmp eax, 0
    je RN_SKIP_FREE
    push eax
    call FreeBed
    pop eax
RN_SKIP_FREE:

    mov eax, totalPatients
    dec eax
    cmp ecx, eax
    je RN_REMOVE_LAST

    ; move last record to current
    mov ebx, eax ; lastIndex

    ; copy name
    mov edx, ebx
    imul edx, MAX_NAME_LEN
    lea esi, patientNames
    add esi, edx
    mov edx, ecx
    imul edx, MAX_NAME_LEN
    lea edi, patientNames
    add edi, edx
    mov ecx, MAX_NAME_LEN
    cld
    rep movsb

    ; copy remaining fields
    mov eax, [patientAges + ebx*4]
    mov [patientAges + ecx*4], eax
    mov eax, [patientBeds + ebx*4]
    mov [patientBeds + ecx*4], eax
    mov eax, [patientDays + ebx*4]
    mov [patientDays + ecx*4], eax
    mov eax, [patientService + ebx*4]
    mov [patientService + ecx*4], eax
    mov eax, [patientDocFee + ebx*4]
    mov [patientDocFee + ecx*4], eax
    mov eax, [patientTotal + ebx*4]
    mov [patientTotal + ecx*4], eax

    ; update bedOwner mapping if moved record has bed
    mov eax, [patientBeds + ecx*4]
    cmp eax, 0
    je RN_SKIP_OWNER_UPDATE
    dec eax
    lea edi, bedOwner
    mov [edi + eax*4], ecx
RN_SKIP_OWNER_UPDATE:

RN_REMOVE_LAST:
    mov eax, totalPatients
    dec eax
    mov totalPatients, eax

    mov edx, OFFSET msgRemoved
    call WriteString
    call Crlf

RN_DONE_POP:
    mov edx, OFFSET msgPressAny
    call WriteString
    call ReadChar
    popad
    ret
RemovePatient ENDP

; ==========================================================
; Main Program
; ==========================================================
main PROC
    call DisplayBanner

    mov edx, OFFSET usernameMsg
    call WriteString
    mov edx, OFFSET tempName
    mov ecx, MAX_NAME_LEN
    call ReadString

    mov al, [tempName]
    cmp al, 'a'
    je ADMIN_MENU
    cmp al, 'u'
    je USER_MENU
    jmp ADMIN_MENU

; -------------------------
; Admin Menu
; -------------------------
ADMIN_MENU:
admin_loop:
    call Crlf
    call DisplayBanner
    mov edx, OFFSET menuText
    call WriteString
    call ReadInt

    cmp eax, 1
    je L_ADD
    cmp eax, 2
    je L_VIEW
    cmp eax, 3
    je L_BEDS
    cmp eax, 4
    je L_BILL
    cmp eax, 5
    je L_REMOVE
    cmp eax, 6
    je L_EXIT
    jmp admin_loop

L_ADD:
    call AddPatient
    jmp admin_loop

L_VIEW:
    call ViewPatients
    jmp admin_loop

L_BEDS:
    call ViewAllBeds
    jmp admin_loop

L_BILL:
    call GenerateBill
    jmp admin_loop

L_REMOVE:
    call RemovePatient
    jmp admin_loop

L_EXIT:
    exit

USER_MENU:
user_loop:
    call Crlf
    call DisplayBanner
    mov edx, OFFSET menuText
    call WriteString
    call ReadInt

    cmp eax, 2
    je L_VIEWU
    cmp eax, 3
    je L_BEDSU
    cmp eax, 4
    je L_BILLU
    cmp eax, 6
    je L_EXITU
    jmp user_loop

L_VIEWU:
    call ViewPatients
    jmp user_loop

L_BEDSU:
    call ViewAllBeds
    jmp user_loop

L_BILLU:
    call GenerateBill
    jmp user_loop

L_EXITU:
    exit

main ENDP

END main

