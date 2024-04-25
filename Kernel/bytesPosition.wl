FunctionCompile[Function[{
	Typed[byteArray, "NumericArray"::["UnsignedInteger8", 1]], 
	Typed[subByteArray, "NumericArray"::["UnsignedInteger8", 1]], 
	Typed[n, "PackedArray"::["MachineInteger", 1]]
}, 
	Module[{m = 0, j = 1, len = Length[subByteArray], positions = {}},
		Do[
			If[
				byteArray[[i ;; i + len - 1]] === subByteArray, 
					m++; 
					If[m === n[[j]], 
						j++; 
						positions = Append[positions, {i, i + len - 1}];  
						If[j > Length[n], Break[]]
					]
			], 
			{i, 1, Length[byteArray] - len + 1}
		]; 
		
		(*Return: {{_Integer, _Integer}.., }*)
		positions
	]
]]