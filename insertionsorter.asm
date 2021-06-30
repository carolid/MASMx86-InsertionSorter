INCLUDE Irvine32.inc

LO = 10
HI = 29
ARRAYSIZE = 200

.data

headlineTitle			    BYTE		"Generating, Sorting, and Counting Random Integers! Programmed by Caroline Davis.", 13,10,0
introDescription_1		BYTE		"This program generates ", 0 
introDescription_2		BYTE		" random numbers between ", 0 
introDescription_3		BYTE		"The program will display: ",13,10
						          BYTE		"1. The original list ",13,10
						          BYTE		"2. The same list, sorted and in ascending order",13,10
andWord					      BYTE		" and ", 0
unsortedMessage			  BYTE		"List of unsorted random numbers: ", 13,10,0
sortedMessage			    BYTE		"List of sorted numbers: ", 13,10,0
goodbyeMessage			  BYTE		"Goodbye, and thanks for using this program!", 0
randArray				      DWORD		ARRAYSIZE DUP(?)
counts					      DWORD		20 DUP(?)
exchangeCount			    DWORD		?
swapValueAddress		  DWORD		?
isCountArray			    DWORD		?

.code

;--------------------------------------------------------------------------------
; Name: main
; 
; Functionality: PUSH all parameters onto the call stack
;	All strings and output values are PUSHed by reference
;	CALLs all sub-procedures
;
; Preconditions: Program is running
;
; Postconditions: Exit to OS
;
; Receives: value of ESP, variable memory addresses from .data section
;
; Returns: return addresses for each called procedure --> any changes made within
;	the sub-procedures is technically sourced back to the call from main
; --------------------------------------------------------------------------------
main PROC
	
	PUSH	OFFSET headlineTitle			
	PUSH	OFFSET introDescription_1		
	PUSH	OFFSET introDescription_2		
	PUSH	OFFSET andWord					
	PUSH	OFFSET introDescription_3		
	CALL	introduction

;-------------------------------------------------------------------
	
	PUSH	OFFSET randArray				
	CALL	fillArray

;--------------------------------------------------------------------
	
	
	PUSH	isCountArray
	PUSH	OFFSET randArray				
	PUSH	OFFSET unsortedMessage			
	CALL	displayList

;--------------------------------------------------------------------
	
	PUSH	OFFSET randArray
	PUSH	OFFSET exchangeCount
	PUSH	OFFSET swapValueAddress
	CALL	sortList

;--------------------------------------------------------------------

	PUSH	isCountArray
	PUSH	OFFSET randArray
	PUSH	OFFSET sortedMessage
	CALL	displayList

;--------------------------------------------------------------------

PUSH	OFFSET goodbyeMessage
	CALL	goodbye

	Invoke ExitProcess,0
main ENDP


;--------------------------------------------------------------------------------
; Name: introduction
; 
; Functionality: Displays the title, programmers name, and instructions to the console
;
; Preconditions: Called from main
;
; Postconditions: ESP changed
;
; Receives: Constants ARRAYSIZE, LO, and HI
;	Many string parameters PUSHed by reference from main off of the data segment, including:
;		1. headlineTitle			
;		2. introDescription_1		
;		3. introDescription_2		
;		4. andWord					
;		5. introDescription_3	
;
; Returns: All registers used are preserved/restored
; --------------------------------------------------------------------------------
introduction PROC	USES EBP EDX EAX
	MOV		EBP, ESP

	MOV		EDX, [EBP + 32]
	CALL	WriteString
	MOV		EDX, [EBP + 28]
	CALL	WriteString
	MOV		EAX, ARRAYSIZE
	CALL	WriteDec
	MOV		EDX, [EBP + 24]
	CALL	WriteString
	MOV		EAX, LO
	CALL	WriteDec
	MOV		EDX, [EBP + 20]
	CALL	WriteString
	MOV		EAX, HI
	CALL	WriteDec
	MOV		AL, 2Eh
	CALL	WriteChar
	MOV		EDX, [EBP + 16]
	CALL	WriteString
	MOV		EAX, LO
	CALL	WriteDec
	MOV		AL, 2Eh
	CALL	WriteChar
	CALL	CrLf

	RET		20

introduction ENDP


;--------------------------------------------------------------------------------
; Name: fillArray
; 
; Functionality: Generates ARRAYSIZE number of random numbers, and fills the array -
;	randArray with them as they are generated. Uses a LOOP for this task - loops
;	ARRAYSIZE number of times. Using Irvine's "RandomRange", set upper Range limit as
;	HI + 1 (to include HI in the range). Set conditional jump to disregard numbers below
;	the LO limit.
;
; Preconditions: Called from main - Uses IrvineLibrary procedures "Randomize" and "RandomRange"
;
; Postconditions: ESP changed
;
; Receives: Constants: ARRAYSIZE, LO, HI;  OFFSET randArray PUSH from main
;
; Returns: Filled array randArray
; --------------------------------------------------------------------------------
fillArray PROC USES EBP EAX ESI ECX EDX EDI
	MOV		EBP, ESP

	MOV		ECX, ARRAYSIZE
	MOV		EDI, [EBP + 28]				; set EDI to OFFSET randArray
	MOV		EDX, 1
	ADD		EDX, HI	
	CALL	Randomize

; ----------------------------------------------------------------
;	_fillLoop fills randArray with ARRAYSIZE number
;		of randomly gnerated numbers. The counter (ECX)
;		has been set to ARRAYSIZE, so the LOOP will repeat
;		that many times. However, the LOOP counter is only
;		decremented if a number within the range [LO, HI]
;		is added to randArray. If the number generated is 
;		below LO, it will simply JMP to the top of the loop,
;		therefore bypass the LOOP directive. EDI starts at the OFFSET
;		of randArray, and increments by 4 (one DWORD) each time
;		a number is added.
; ----------------------------------------------------------------
_fillLoop:
	MOV		EAX, EDX
	CALL	RandomRange
	CMP		EAX, LO
	JL		_belowLO
	MOV		[EDI], EAX
	ADD		EDI, 4						; advance EDI one DWORD (data size of randArray elements)
	LOOP	_fillLoop
	JMP		_endFillLoop

_belowLO:
	JMP		_fillLoop
	
_endFillLoop:
	CALL	CrLf
	RET		4

fillArray ENDP


;--------------------------------------------------------------------------------
; Name: sortList
; 
; Functionality: Uses an insertion sorting algorithm to sort randArray in ascending order as follows:
;	1. Uses a nested LOOP - outer loop counter set to ARRAYSIZE, inner loop counter increment for each
;		iteration of outer loop - starting at 1
;	2. Sets the two indices being evaluated each iteration - EBX starts at i = [1], EAX at i =[0]
;	3. The inner loop counter - EDX (PUSHed, then POPed after outer loop completion, then moved into ECX prior to inner loop)
;		is equivalent to the current index being evaluated. Therefore, EDX * 4 == current [i] element
;	4. The inner loop calls exchangeElements PROC, then after its execution --> decrements the index being evaluated against EBX
;	3. Uses a sub-procedure - exchangeElements - to compare and swap elements in randArray
;
; Preconditions: Called from main - uses the values within randArray and exchangeCount
;
; Postconditions: Changes ESP
;
; Receives: Constant: ARRAYSIZE;  OFFSET randArray, swapValueAddress (used by exchangeElements), and
;	exchangeCount - all PUSHed from main
;
; Returns:
;	To main -
;		1. Sorted randArray
;	To exchangeElements - 
;		1. exchangeCount
;		2. randArray - first index being evaluated (EBX)
;		3. randArray - second index being evaluated (EAX)
;		4. Inner loop counter (ECX --> derived from EDX)
; --------------------------------------------------------------------------------
sortList PROC USES EBP EAX ECX EDX EBX EDI
	MOV		EBP, ESP

	MOV		ECX, ARRAYSIZE
	SUB		ECX, 1
	MOV		EDX, 1

; ----------------------------------------------------------------
;	sortOuterLoop initiates the insertion sort process. It first
;		sets ESI to the OFFSET of exchangeCount which will count
;		the number of exchanges the sort function performs. exchangeCount
;		is reset to 0 each time the outerLoop iterates. 
;	Then, ECX and EDX are PUSHed prior to the setting of the indices for 
;		the innerLoop. EDX will hold the count for ECX for the innerLoop.
;	The outerLoop counter is set to ARRAYSIZE-1. The innerLoop counter 
;		initiates at 1, then increments for each outerLoop iteration. This 
;		is because, if the program is evaluating the value at index[51] of 
;		randArray, it needs to evaluate if the previous 50 indices hold values 
;		that are greater than the current value.
;	EBX will hold the value at the index that is being compared to all of the previous
;		values. So it is initially set to the OFFSET of randArray + 4 * COUNT(1) = index[1].
;	EAX holds the value currently being compared to EBX, thus it starts at OFFSET randArray =
;		index[0].
;	EDX is POPed and moved into ECX - this initiates the counter for the innerLoop.
;	The innerLoop CALLs exchange elements, and decrements the index being moved into EAX until
;		the counter reaches 0.
; ----------------------------------------------------------------
_sortOuterLoop:
	MOV		ESI, [EBP + 32]				; OFFSET exchangeCount
	MOV		EAX, 0
	MOV		[ESI], EAX
		
	PUSH	ECX
	PUSH	EDX

	; ----------------------------------------------------------------
	; This section generates the memory address of the index
	;	the program is currently trying to reach. This is 
	;	equivalent to the count (EDX) * 4. Because 4 is the
	;	data size of each randArray element - DWORD.
	; ----------------------------------------------------------------
_calculateAddressCurrentIndex:		
	MOV		EAX, EDX
	MOV		EBX, 4	
	MUL		EBX

_setFirstIndex:
	MOV		EDI, [EBP + 36]				; OFFSET randArray
	ADD		EDI, EAX
	MOV		EBX, [EDI]

_setSecondIndex:
	SUB		EDI, 4			
	MOV		EAX, [EDI]

_setInnerLoopCount:
	POP		EDX
	MOV		ECX, EDX

_sortInnerLoop:
	CALL	exchangeElements
	SUB		EDI, 4	
	MOV		EAX, [EDI]
	LOOP	_sortInnerLoop

	INC		EDX
	POP		ECX
	LOOP	_sortOuterLoop

_endSortLoop:
	RET		16

sortList ENDP


;--------------------------------------------------------------------------------
; Name: exchangeElements
; 
; Functionality: A sub-procedure to the sortList PROC. Evaluates whether EAX is greater than EBX.
;	If yes --> stores the current index address where the swap is occuring, and EAX replaces EBX at
;		its current index location
;	If no --> evaluates if EBX needs to be replaced in the list (this means that the EBX value has been
;		replaced by the EAX value one or more times in the process of swapping index locations). This is
;		known because the sortList inner loop will have no more iteration in the current outer loop iteration 
;		(ECX == 1), and the exchangeCount value will be greater than 0. 
;		If either of these cases are true...
;			The memory address being stored in swapValueAddress will be pulled out and into ESI, then the value 
;			in EBX will replace the value at the swapValueAddress index.
;
;
; Preconditions: Called from sortList PROC - Uses the values in EAX, EBX, ECX, and uses the values PUSHed to 
;	sortList from main PROC
;
; Postconditions: ESP, and possibly exchangeCount, and swapValueAddress changed
;
; Receives: EAX, EBX, ECX, exchangeCount, and swapValueAddress
;
; Returns: exchangeCount, swapValueAddress, EAX, EBX, and modified randArray
; --------------------------------------------------------------------------------
exchangeElements PROC USES EDI ESI EDX
	MOV		EDX, 1
	CMP		EAX, EBX
	JG		_exchangeNumsStoreAddress
	JMP		_swapValues

; ----------------------------------------------------------------
;	This part of the program occurs after it has been determined that
;		EAX is greater than EBX - this means these two elements are out
;		of asccending order. It stores the memory address where a swap
;		is occurring. Thus further in the program, the last address
;		of swap will still be available. Then the address is incremented
;		by one index, and the value in EAX replaces the value in EBX at
;		that index of randArray. This results in a duplication of the value
;		in EAX, and the value in EBX is now missing from the array, but
;		still stored in EBX.
;	Also, exchangeCount is incremeneted by 1 to signify that an exchange
;		has occurred. 
; ----------------------------------------------------------------
_exchangeNumsStoreAddress:
	MOV		ESI, [EBP + 28]				; OFFSET swapValueAddress
	MOV		[ESI], EDI
	ADD		EDI, 4	
	MOV		[EDI], EAX
	MOV		ESI, [EBP + 32]				; OFFSET exchangeCount
	ADD		[ESI], EDX				
		

; ----------------------------------------------------------------
;	swapValues evaluates if the value in EBX needs to be replaced 
;		in the list. It does this in 2 ways.
;	1. If the counter == 1, this means the program has iterated through 
;		all of the values in the array prior to where EBX used to reside. 
;	2. If exchangeCount is not 0. This means at least 1 exchange has
;		occurred. 
;	If both of these cases are True, then the value in EBX is not currently 
;		represented in randArray, and it needs to be replaced. This is where
;		the swapValueAddress value comes into play. EBX will be placed at
;		the memory address within swapValueAddress. This is where the last
;		swap occurred - the memory location prior to the most recent EAX value
;		duplication.
; ----------------------------------------------------------------
_swapValues:
	CMP		ECX, 1
	JNE		_returnToLoop
	MOV		ESI, [EBP + 32]
	MOV		EDX, [ESI]
	CMP		EDX, 0
	JE		_returnToLoop
	MOV		ESI, [EBP + 28]	
	MOV		EDI, [ESI]
	MOV		[EDI], EBX
	
_returnToLoop:
	RET

exchangeElements ENDP



;--------------------------------------------------------------------------------
; Name: displayList
; 
; Functionality: Displays the various lists throughout the program - unsorted random array,
;	sorted random array, and counted list. Writes the string to console to describe which list
;	is being displayed, and evaluates whether the list to be displayed is the count array.
;	If so, EBX = HI - LO, and each iteration of the inner loop DEC EBX, and evaluates if EBX == 0.
;	Once EBX == 0 --> the loop ends. In the other cases that displayList is called, EBX is set to
;	ARRAYSIZE. The inner loop count is set to 20, so after each 20 numbers, the cursor moves to 
;	a new line.
;
; Preconditions: Called from main - uses each version of randArray - sorted and unsorted, and uses
;	counts array. 
;
; Postconditions: Changes ESP
;
; Receives: randArray and counts array; CONSTANTS: LO, HI, ARRAYSIZE
;
; Returns: None - prints to console; all registers are preserved/restored
; --------------------------------------------------------------------------------
displayList PROC USES EBP EAX EDX ECX EBX EDI
	MOV		EBP, ESP

	MOV		EAX, [EBP + 36]				; isCountArray value
	MOV		EDI, [EBP + 32]				; OFFSET randArray
	MOV		EDX, [EBP + 28]				; OFFSET message
	CALL	WriteString

	MOV		EBX, ARRAYSIZE
	MOV		ECX, 20
	CMP		EAX, 0
	JE		_printOuterLoop	
	
; ----------------------------------------------------------------
;	displayCountsArray runs after the value in isCountArray is evaluated.
;		if the value is 0, the list to be displayed is not counts. If
;		the value is 1, then the list being displayed is counts.
;	If the list is the counts array, then the program will only display 
;		the numbers in counts array. The number of values in the counts array
;		is dependent on the width of the range. Therefore, HI - LO = len(counts)
;	Each time the printLoop iterates, it evaluates if EBX is 0. The outerLoop would 
;		usually iterate ARRAYSIZE times, however, when isCountArray == 1, EBX == len(counts),
;		and the loop only iterates len(counts) amount of times. So if 20 numbers needs to be 
;		displayed, the loop repeats only 20 times.  
; ----------------------------------------------------------------
_displayCountsArray:
	MOV		EAX, LO
	CALL	WriteDec
	MOV		AL, 3Ah
	CALL	WriteChar
	CALL	CrLf
	MOV		EAX, LO
	MOV		EBX, HI
	SUB		EBX, EAX

_printOuterLoop:
	PUSH	ECX
	MOV		ECX, 20
_printArrayLoop:
	CMP		EBX, 0
	JE		_endPrintLoop
	MOV		EAX, [EDI]
	CALL	WriteDec
	MOV		AL, 20h
	CALL	WriteChar
	ADD		EDI, 4
	DEC		EBX
	LOOP	_printArrayLoop
		
	POP		ECX
	CALL	CrLf
	LOOP	_printOuterLoop

_endPrintLoop:
	POP		ECX
	CALL	CrLf
	CALL	CrLf
	RET		8

displayList	ENDP


;--------------------------------------------------------------------------------
; Name: goodbye
; 
; Functionality: Displays a goodbye message to the console before the program ends
;
; Preconditions: Called from main - OFFSET goodbyeMessage PUSHed from main
;
; Postconditions: Changes ESP
;
; Returns: None - returns to main
; --------------------------------------------------------------------------------
goodbye PROC USES EBP EDX
	MOV		EBP, ESP

	MOV		EDX, [EBP + 12]
	CALL	WriteString
	CALL	CrLf

	RET		4

goodbye ENDP


END main
