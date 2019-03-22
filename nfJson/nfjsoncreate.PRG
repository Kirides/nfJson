*-------------------------------------------------------------------
* Created by Marco Plaza @nfTools
* ver 1.100 - 24/02/2016
* enabled collection processing
* ver 1.101 - 24/02/2016
* solved indent on nested collections
* ver 1.110 -11/03/2016
* -added support for collections inside arrays
* -user can pass aMemembersFlag value
*  (since Json is intended for DTO creation default value is 'U')
*   check amembers topic on vfp help file for usage
* changed cr to crlf
* Added Json validation ; throws error for invalid Json.
* ver 1.120
* encode control characters (chr(0) ~ chr(31))
* ver 1.130
* remove suffix from serialized collection, making it a simple JSON-Array
* ver 1.131
* Refactor naming, order of methods, put #DEFINE at top
*-----------------------------------------------------------------------
#DEFINE crlf CHR(13) + CHR(10)
#DEFINE PAD_SEQUENCE '  '
#DEFINE specialChars ["\/] + CHR(127) + CHR(12) + CHR(10) + CHR(13) + CHR(9) + CHR(0) + CHR(1) + CHR(2) + CHR(3) + CHR(4) + CHR(5) + CHR(6) + CHR(7) + CHR(8) + CHR(9) + CHR(10) + CHR(11) + CHR(12) + CHR(13) + CHR(14) + CHR(15) + CHR(16) + CHR(17) + CHR(18) + CHR(19) + CHR(20) + CHR(21) + CHR(22) + CHR(23) + CHR(24) + CHR(25) + CHR(26) + CHR(27) + CHR(28) + CHR(29) + CHR(30) + CHR(31)

PARAMETERS ovfp, FORMATTEDOUTPUT, nonullarrayitem, crootName, aMembersFlag
PRIVATE ALL

aMembersFlag = EVL(m.aMembersFlag, 'U')
isArray = TYPE('oVfp', 1) = 'A'
isObject = VARTYPE(m.ovfp) = 'O'
IF !m.isArray AND !m.isObject
	ERROR 'must supply a vfp object/array'
	RETURN
ENDIF
_indentation = IIF(CAST(m.formattedOutput AS l) , 1, -1)
lcPoint = SET('Point')
SET POINT TO '.'
DO CASE
	CASE isArray
		ojson = CREATEOBJECT('empty')
		ADDPROPERTY(ojson, 'array(1)')
		ACOPY(ovfp, ojson.ARRAY)
		cjson = fromObject(ojson, .F., m.nonullarrayitem, m.aMembersFlag)
		cjson = SUBSTR(m.cjson, AT('[', m.cjson))

	CASE TYPE('oVfp.BaseClass')='C' AND ovfp.BASECLASS = 'Collection'
		cjson = fromObject(ovfp, .T., m.nonullarrayitem, m.aMembersFlag)
	OTHERWISE
		cjson = '{' + fromObject(ovfp, .F., m.nonullarrayitem, m.aMembersFlag) + '}'
ENDCASE
SET POINT TO (m.lcPoint)
RETURN LTRIM(m.cjson)

*----------------------------------------------------------------------------------
FUNCTION fromObject(obt, isCollection, nonullarrayitem, aMembersFlag)
*----------------------------------------------------------------------------------
	IF ISNULL(m.obt)
		RETURN 'null'
	ENDIF
	PRIVATE ALL EXCEPT _indentation
	cCurrent = ''
	xtabs = indent(2)
	bc = IIF(TYPE('m.obt.baseclass')='C', m.obt.BASECLASS, '?')
	isCollection =  bc = 'Collection'
	IF m.isCollection
		cCurrent = m.cCurrent + '[' + m.xtabs
		fromCollection(obt, m.nonullarrayitem, m.aMembersFlag)
		cCurrent  = m.cCurrent + m.xtabs + ']'
	ELSE
		AMEMBERS(am, m.obt, 0, m.aMembersFlag)
		IF VARTYPE(m.am) = 'U'
			xtabs = m.indent(-2)
			RETURN ''
		ENDIF

		nm = ALEN(am)
		FOR x1 = 1 TO m.nm
			VAR = LOWER(am(m.x1))
			cCurrent = m.cCurrent + IIF(m.x1>1, ', ', '') + m.xtabs
			cCurrent = m.cCurrent + ["] + STRTRAN(m.var, '_vfpsafe_', '') + [":]
			isObject = TYPE('m.obt.&Var')='O'
			IF TYPE('m.obt.&var') = 'U'
				cCurrent = m.cCurrent + ["unable to evaluate expression"]
				LOOP
			ENDIF
			isArray  = TYPE('m.obt.&Var', 1) = 'A'
			DO CASE
				CASE m.isArray
					fromArray(m.obt, m.var, m.nonullarrayitem)
				CASE m.isObject
					thiso = m.obt.&VAR
					bc = IIF(TYPE('m.thiso.baseclass')='C', m.thiso.BASECLASS, '?')
					IF bc = 'Collection'
						cCurrent =  m.cCurrent + fromObject(m.obt.&VAR, .T., m.nonullarrayitem, m.aMembersFlag)
					ELSE
						cCurrent =  m.cCurrent + [{] + fromObject(m.obt.&VAR, .F., m.nonullarrayitem, m.aMembersFlag) + [}]
					ENDIF
				OTHERWISE

					cCurrent = m.cCurrent + toJsonValue(m.obt.&VAR)
			ENDCASE
		ENDFOR
	ENDIF
	xtabs = indent(-2)
	cCurrent  = m.cCurrent + m.xtabs

	RETURN m.cCurrent
ENDFUNC

*----------------------------------------------------
FUNCTION fromArray(obt, arrayName, nonullarrayitem)
*----------------------------------------------------
	nrows = ALEN(m.obt.&arrayName, 1)
	ncols = ALEN(m.obt.&arrayName, 2)
	bidim = m.ncols > 0
	ncols = IIF(m.ncols = 0, m.nrows, m.ncols)
	titems = ALEN(m.obt.&arrayName)
	xtabs = indent(2)
	cCurrent = m.cCurrent + '[' + m.xtabs
	nelem = 1
	DO WHILE m.nelem <= m.titems
		cCurrent = m.cCurrent + IIF(m.nelem>1, ', ' + m.xtabs, '')
		IF m.bidim
			xtabs = indent(2)
			cCurrent = m.cCurrent + '[' + m.xtabs
		ENDIF
		FOR pn = m.nelem TO m.nelem + m.ncols-1
			elem = m.obt.&arrayName(m.pn)
			cCurrent = m.cCurrent + IIF(m.pn>m.nelem, ', ' + m.xtabs, '')
			IF VARTYPE(m.elem) # 'O'
				IF m.nelem + m.ncols-1 = 1 AND ISNULL(m.elem) AND m.nonullarrayitem
					cCurrent = m.cCurrent + ""
				ELSE
					cCurrent = m.cCurrent + toJsonValue(m.elem)
				ENDIF
			ELSE
				bc = IIF(TYPE('m.elem.baseclass')='C', m.elem.BASECLASS, '?')
				IF bc = 'Collection'
					cCurrent =  m.cCurrent + fromObject(m.elem ,.T., m.nonullarrayitem, m.aMembersFlag)
				ELSE
					cCurrent =  m.cCurrent + [{] + fromObject(m.elem ,.F., m.nonullarrayitem, m.aMembersFlag) + [}]
				ENDIF
			ENDIF
		ENDFOR
		nelem = m.pn
		IF m.bidim
			xtabs = indent(-2)
			cCurrent = m.cCurrent + m.xtabs + ']'
		ENDIF
	ENDDO

	xtabs = indent(-2)
	cCurrent = m.cCurrent + m.xtabs + ']'
ENDFUNC

*---------------------------------------------------------------
FUNCTION fromCollection(obt, nonullArrayItems, aMembersFlag)
*---------------------------------------------------------------
	LOCAL isCollection
	WITH obt
		nm = .COUNT
		isKeyValue = .COUNT > 0 AND !EMPTY(.GETKEY(1))
		FOR x1 = 1 TO .COUNT
			IF m.isKeyValue
				elem = CREATEOBJECT('empty')
				ADDPROPERTY(elem, 'Key', .GETKEY(m.x1))
				ADDPROPERTY(elem, 'Value', .ITEM(m.x1))
			ELSE
				elem = .ITEM(m.x1)
			ENDIF
			cCurrent = m.cCurrent + IIF(m.x1>1, ', ' + m.xtabs, '')
			IF VARTYPE(m.elem) # 'O'
				cCurrent = m.cCurrent + toJsonValue(m.elem)
			ELSE
				IF VARTYPE(m.elem.BASECLASS) = 'C' AND m.elem.BASECLASS = 'Collection'
					isCollection = .T.
				ELSE
					isCollection = .F.
					m.cCurrent = m.cCurrent + '{'
				ENDIF
				cCurrent = m.cCurrent + fromObject(m.elem, m.isCollection , m.nonullarrayitem, m.aMembersFlag)
				IF !m.isCollection
					cCurrent = m.cCurrent + '}'
				ENDIF
			ENDIF
		ENDFOR
		cCurrent = RTRIM(m.cCurrent, 1, m.xtabs)
	ENDWITH
ENDFUNC

*-----------------------------
FUNCTION indent(nLength)
*-----------------------------
	IF m._indentation = -1
		RETURN ''
	ELSE
		_indentation= m._indentation + m.nLength
		RETURN crlf + REPLICATE(PAD_SEQUENCE, m._indentation)
	ENDIF
ENDFUNC

*-----------------------------
FUNCTION toJsonValue(eValue)
*-----------------------------
	IF ISNULL(m.eValue)
		RETURN 'null'
	ELSE
		tvar = VARTYPE(m.eValue)
		** do NOT change the order!
		DO CASE
			CASE m.tvar $ 'FBYINQ'
				vc = RTRIM(CAST(m.eValue AS c(32)))
			CASE m.tvar = 'L'
				vc = IIF(m.eValue, 'true', 'false')
			CASE m.tvar $ 'DT'
				vc = ["] + TTOC(m.eValue, 3) + [Z"]
			CASE mustEncode(m.eValue)
				vc = ["] + escapeandencode(m.eValue) + ["]
			CASE m.tvar $ 'CVM'
				vc = ["] + m.eValue + ["] && Don't RTRIM "excess" space.
			CASE m.tvar $ 'GQW'
				vc = ["] + STRCONV(m.eValue, 13) + ["]
		ENDCASE
		RETURN m.vc
	ENDIF
ENDFUNC

*-----------------------------------
FUNCTION mustEncode(eValue)
*-----------------------------------
	RETURN LEN(CHRTRAN(m.eValue, specialChars, '')) <> LEN(m.eValue)
ENDFUNC

*-------------------------------
FUNCTION escapeandencode(cValue)
*-------------------------------
	cValue = STRTRAN(m.cValue, '\', '\\')
	cValue = STRTRAN(m.cValue, '"', '\"')
	*cValue = Strtran(m.cValue, '/', '\/')
	IF !mustEncode(m.cValue) && optimize perf.
		RETURN
	ENDIF
	cValue = STRTRAN(m.cValue, CHR(127), '\b')
	cValue = STRTRAN(m.cValue, CHR(12), '\f')
	cValue = STRTRAN(m.cValue, CHR(10), '\n')
	cValue = STRTRAN(m.cValue, CHR(13), '\r')
	cValue = STRTRAN(m.cValue, CHR(9), '\t')
	IF !mustEncode(m.cValue)
		RETURN
	ENDIF
	LOCAL x
	FOR x = 0 TO 31
		cValue = STRTRAN(m.cValue, CHR(m.x), '\u' + RIGHT(TRANSFORM(m.x, '@0'), 4))
	ENDFOR
	RETURN RTRIM(m.cValue)
ENDFUNC
