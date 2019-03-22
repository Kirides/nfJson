*-------------------------------------------------------------------
* Created by Marco Plaza , 2013-2017 @nfTools
*-------------------------------------------------------------------
#DEFINE crlf CHR(13)+CHR(10)

LPARAMETERS cjsonstr
PRIVATE ALL

stacklevels=ASTACKINFO(aerrs)
IF m.stacklevels > 1
	calledfrom = ' ( called From '+aerrs(m.stacklevels-1,4)+' line '+TRANSFORM(aerrs(m.stacklevels-1,5))+')'
ELSE
	calledfrom = ''
ENDIF
TRY
	cerror = ''
	IF NOT LEFT(LTRIM(cjsonstr),1)  $ '{['  AND FILE(m.cjsonstr)
		cjsonstr = FILETOSTR(m.cjsonstr)
	ENDIF
	ost = SET('strictdate')
	SET STRICTDATE TO 0
	ojson = nfjsonread2(m.cjsonstr)
	SET STRICTDATE TO (m.ost)
CATCH TO oerr1
	cerror = 'nfJson '+m.calledfrom+crlf+m.oerr1.MESSAGE
ENDTRY
IF !EMPTY(m.cerror)
	ERROR m.cerror
	RETURN .NULL.
ENDIF
RETURN IIF(VARTYPE(m.ojson)='O',m.ojson,.NULL.)

*-------------------------------------------------------------------------
FUNCTION nfjsonread2(cjsonstr)
*-------------------------------------------------------------------------
	TRY
		x = 1
		cerror = ''
* process json:
		cjson = RTRIM(CHRTRAN(m.cjsonstr,CHR(13)+CHR(9)+CHR(10),''))
		pchar = LEFT(LTRIM(m.cjson),1)
		nl = ALINES(aj,m.cjson,20,'{','}','"',',',':','[',']','\\')
		FOR xx = 1 TO ALEN(aj)
			IF LEFT(LTRIM(aj(m.xx)),1) $ '{}",:[]'  OR LOWER(LEFT(LTRIM(m.aj(m.xx)),4)) $ 'true/false/null'
				aj(m.xx) = LTRIM(aj(m.xx))
			ENDIF
		ENDFOR
		ostack = CREATEOBJECT('stack')
		ojson = CREATEOBJECT('empty')
		DO CASE
			CASE  aj(1)='{'
				x = 1
				ostack.pushobject()
				procstring(m.ojson)
			CASE aj(1) = '['
				x = 0
				procstring(m.ojson,.T.)
			OTHERWISE
				ERROR ' expecting [{  got '+m.pchar
		ENDCASE
	CATCH TO oerr
		strp = ''
		FOR Y = 1 TO m.x
			strp = m.strp+aj(m.y)
		ENDFOR
		DO CASE
			CASE oerr.ERRORNO = 1098
				cerror = ' Invalid Json: '+ m.oerr.MESSAGE+crlf+' Parsing: '+RIGHT(m.strp,80)
			OTHERWISE
				cerror = ' program error # '+TRANSFORM(m.oerr.ERRORNO)+crlf+m.oerr.MESSAGE+' at line: '+TRANSFORM(oerr.LINENO)+crlf+' Parsing: '+RIGHT(m.strp,80)
		ENDCASE
	ENDTRY
	IF !EMPTY(m.cerror)
		ERROR m.cerror
	ENDIF
	RETURN m.ojson
ENDFUNC

*--------------------------------------------------------------------------------
PROCEDURE procstring(obj,evalue)
*--------------------------------------------------------------------------------
	#DEFINE cvalid 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_'
	#DEFINE creem  '_______________________________________________________________'
	PRIVATE rowpos,colpos,bidim,ncols,arrayname,expecting,arraylevel,vari
	PRIVATE expectingpropertyname,expectingvalue,objectopen
	expectingpropertyname = !m.evalue
	expectingvalue = m.evalue
	expecting = IIF(expectingpropertyname,'"}','')
	objectopen = .T.
	bidim = .F.
	colpos = 0
	rowpos = 0
	arraylevel = 0
	arrayname = ''
	vari = ''
	ncols = 0
	DO WHILE m.objectopen
		x = m.x+1
		DO CASE
			CASE m.x > m.nl
				m.x = m.nl
				IF ostack.COUNT > 0
					ERROR 'expecting '+m.expecting
				ENDIF
				RETURN
			CASE aj(m.x) = '}' AND '}' $ m.expecting
				closeobject()
			CASE aj(x) = ']' AND ']' $ m.expecting
				closearray()
			CASE  m.expecting = ':'
				IF aj(m.x) = ':'
					expecting = ''
					LOOP
				ELSE
					ERROR 'expecting : got '+aj(m.x)
				ENDIF
			CASE ',' $ m.expecting
				DO CASE
					CASE aj(x) = ','
						expecting = IIF( '[' $ m.expecting , '[' , '' )
					CASE NOT aj(m.x) $ m.expecting
						ERROR 'expecting '+m.expecting+' got '+aj(m.x)
					OTHERWISE
						expecting = STRTRAN(m.expecting,',','')
				ENDCASE
			CASE m.expectingpropertyname
				IF aj(m.x) = '"'
					propertyname(m.obj)
				ELSE
					ERROR 'expecting "'+m.expecting+' got '+aj(m.x)
				ENDIF
			CASE m.expectingvalue
				IF m.expecting == '[' AND m.aj(m.x) # '['
					ERROR 'expecting [ got '+aj(m.x)
				ELSE
					procvalue(m.obj)
				ENDIF
		ENDCASE
	ENDDO
ENDPROC

*-----------------------------------------------------------------------------
FUNCTION anuevoel(obj,arrayname,valasig,bidim,colpos,rowpos)
*-----------------------------------------------------------------------------
	rowpos = m.rowpos+1
	obj.&arrayname..ADD(m.valasig)
	IF VARTYPE(m.valasig) = 'O'
		procstring(obj.&arrayname..ITEM(m.rowpos))
	ENDIF
	
*-----------------------------------------
FUNCTION unescunicode( cstr )
*-----------------------------------------
	PRIVATE ALL
	ust = ''
	FOR x = 1 TO ALINES(xstr,m.cstr,18,'\u','\\u')
		IF RIGHT(xstr(m.x),3) # '\\u' AND RIGHT(xstr(m.x),2) = '\u'
			ust = m.ust + RTRIM(xstr(M.x),0,'\u')
			dec = VAL( "0x"+LEFT(xstr(m.x+1),4))
			ANSI = STRCONV( BINTOC( m.dec  , "2RS" ) ,6 )
			IF m.ansi = '?'
				ust = m.ust + '&#'+TRANSFORM(m.dec)
			ELSE
				ust = m.ust + m.ansi
			ENDIF
			xstr(m.x+1) = SUBSTR(xstr(m.x+1),5)
		ELSE
			ust = m.ust + xstr(m.x)
		ENDIF
	ENDFOR
	cstr = m.ust
ENDFUNC

*-----------------------------------
FUNCTION unescapecontrolc( VALUE )
*-----------------------------------
	IF AT('\', m.value) = 0
		RETURN
	ENDIF
* unescape special characters:
	PRIVATE aa,elem,unesc
	DECLARE aa(1)
	=ALINES(m.aa,m.value,18,'\\','\b','\f','\n','\r','\t','\"','\/')
	unesc =''
	#DEFINE sustb 'bnrt/"'
	#DEFINE sustr CHR(127)+CHR(10)+CHR(13)+CHR(9)+CHR(47)+CHR(34)
	FOR EACH elem IN m.aa
		IF ! m.elem == '\\' AND LEFT(RIGHT(m.elem,2),1) = '\'
			elem = LEFT(m.elem,LEN(m.elem)-2)+CHRTRAN(RIGHT(m.elem,1),sustb,sustr)
		ENDIF
		unesc = m.unesc+m.elem
	ENDFOR
	VALUE = m.unesc
ENDFUNC

*--------------------------------------------
FUNCTION propertyname(obj)
*--------------------------------------------
	x = m.x+1
	vari = aj(m.x)
	DO WHILE RIGHT(aj(m.x),1) # '"' AND m.x < ALEN(m.aj)
		x=m.x+1
		vari = m.vari + aj(m.x)
	ENDDO
	IF RIGHT(m.aj(m.x),1) # '"'
		ERROR ' expecting "  got  '+ m.aj(m.x)
	ENDIF
	vari = RTRIM(m.vari,1,'"')
	vari = IIF(ISALPHA(m.vari),'','_')+m.vari
	vari = CHRTRAN( vari, CHRTRAN( vari, cvalid,'' ) , creem )
	IF vari == 'tabindex'
		vari = '_tabindex'
	ENDIF
	expecting = ':'
	expectingvalue = .T.
	expectingpropertyname = .F.
ENDFUNC

*-------------------------------------------------------------
PROCEDURE procvalue(obj)
*-------------------------------------------------------------
	DO CASE
		CASE aj(m.x) = '{'
			ostack.pushobject()
			IF m.arraylevel = 0
				ADDPROPERTY(obj,m.vari,CREATEOBJECT('empty'))
				procstring(obj.&vari)
				expectingpropertyname = .T.
				expecting = ',}'
				expectingvalue = .F.
			ELSE
				anuevoel(m.obj,m.arrayname,CREATEOBJECT('empty'),m.bidim,@m.colpos,@m.rowpos)
				expectingpropertyname = .F.
				expecting = ',]'
				expectingvalue = .T.
			ENDIF
		CASE  aj(x) = '['
			ostack.pusharray()
			DO CASE
				CASE m.arraylevel = 0
					arrayname = EVL(m.vari,"array")
					rowpos = 0
					colpos = 0
					bidim = .F.
					
					TRY
						ADDPROPERTY(obj,(m.arrayname),CREATEOBJECT("Collection"))
					CATCH
						m.arrayname = m.arrayname+'_vfpSafe_'
						ADDPROPERTY(obj,(m.arrayname),CREATEOBJECT("Collection"))
					ENDTRY	
				CASE m.arraylevel = 1 AND !m.bidim
					rowpos = 1
					colpos = 0
					ncols = 1
					bidim = .T.
			ENDCASE
			arraylevel = m.arraylevel+1
			vari=''
			expecting = IIF(!m.bidim,'[]{',']')
			expectingvalue = .T.
			expectingpropertyname = .F.
		OTHERWISE
			isstring = aj(m.x)='"'
			x = m.x + IIF(m.isstring,1,0)
			VALUE = ''
			DO WHILE m.x <= ALEN(m.aj)
				VALUE = m.value + aj(m.x)
				IF  ( ( m.isstring AND RIGHT(aj(m.x),1) = '"' ) OR (!m.isstring AND RIGHT(aj(m.x),1) $ '}],') ) AND LEFT(RIGHT(aj(m.x),2),1) # '\'
					EXIT
				ENDIF
				x = m.x+1
			ENDDO
			closechar = RIGHT(aj(m.x),1)
			VALUE = LEFT(m.value,LEN(m.value)-1)
			DO CASE
				CASE EMPTY(m.value) AND  NOT ( m.isstring AND m.closechar = '"'  )
					ERROR 'Expecting value got '+m.closechar
				CASE  m.isstring
					IF m.closechar # '"'
						ERROR 'expecting " got '+m.closechar
					ENDIF
				CASE ostack.isobject() AND NOT m.closechar $ ',}'
					ERROR 'expecting ,} got '+m.closechar
				CASE ostack.isarray() AND  NOT m.closechar $ ',]'
					ERROR 'expecting ,] got '+m.closechar
			ENDCASE
			IF m.isstring
* don't change this lines sequence!:
				unescunicode(@m.value)  && 1
				unescapecontrolc(@m.value)  && 2
				VALUE = STRTRAN(m.value,'\\','\')  && 3
** check for Json DateTime: && 2017-03-10T17:43:55
* proper formatted dates with invalid values will parse as character - eg: {"today":"2017-99-01T15:99:00"}
				IF isjsondt( m.value )
					VALUE = jsondatetodt( m.value )
				ENDIF
			ELSE
				VALUE = ALLTRIM(m.value)
				DO CASE
					CASE LOWER(m.value) == 'null'
						VALUE = .NULL.
					CASE LOWER(m.value) == 'true' OR LOWER(m.value) == 'false'
						VALUE = m.value='true'
					CASE EMPTY(CHRTRAN(m.value,'-1234567890.Ee',''))
						TRY
							LOCAL tvaln,im
							im = 'tvaln = '+m.value
							&im
							VALUE = m.tvaln
							badnumber = .F.
						CATCH
							badnumber = .T.
						ENDTRY
						IF badnumber
							ERROR 'bad number format:  got '+aj(m.x)
						ENDIF
					OTHERWISE
						ERROR 'expecting "|number|null|true|false|  got '+aj(m.x)
				ENDCASE
			ENDIF
			IF m.arraylevel = 0
				ADDPROPERTY(obj,m.vari,m.value)
				expecting = '}'
				expectingvalue = .F.
				expectingpropertyname = .T.
			ELSE
				anuevoel(obj,m.arrayname,m.value,m.bidim,@m.colpos,@m.rowpos)
				expecting = ']'
				expectingvalue = .T.
				expectingpropertyname = .F.
			ENDIF
			expecting = IIF(m.isstring,',','')+m.expecting
			DO CASE
				CASE m.closechar = ']'
					closearray()
				CASE m.closechar = '}'
					closeobject()
			ENDCASE
	ENDCASE
*------------------------------
FUNCTION closearray()
*------------------------------
	IF ostack.POP() # 'A'
		ERROR 'unexpected ] '
	ENDIF
	IF m.arraylevel = 0
		ERROR 'unexpected ] '
	ENDIF
	arraylevel = m.arraylevel-1
	IF m.arraylevel = 0
		arrayname = ''
		rowpos = 0
		colpos = 0
		expecting = IIF(ostack.isobject(),',}','')
		expectingpropertyname = .T.
		expectingvalue = .F.
	ELSE
		IF  m.bidim
			rowpos = m.rowpos+1
			colpos = 0
			expecting = ',]['
		ELSE
			expecting = ',]'
		ENDIF
		expectingvalue = .T.
		expectingpropertyname = .F.
	ENDIF
ENDFUNC
*-------------------------------------
FUNCTION closeobject()
*-------------------------------------
	IF ostack.POP() # 'O'
		ERROR 'unexpected }'
	ENDIF
	IF m.arraylevel = 0
		expecting = ',}'
		expectingvalue = .F.
		expectingpropertyname = .T.
		objectopen = .F.
	ELSE
		expecting = ',]'
		expectingvalue = .T.
		expectingpropertyname = .F.
	ENDIF
ENDFUNC
*----------------------------------
FUNCTION isjsondt( cstr )
*----------------------------------
	cstr = RTRIM(m.cstr,1,'Z')
	RETURN IIF( LEN(m.cstr) = 19 ;
		AND LEN(CHRTRAN(m.cstr,'01234567890:T-','')) = 0 ;
		AND SUBSTR(m.cstr,5,1) = '-' ;
		AND SUBSTR(m.cstr,8,1) = '-' ;
		AND SUBSTR(m.cstr,11,1) = 'T' ;
		AND SUBSTR(m.cstr,14,1) = ':' ;
		AND SUBSTR(m.cstr,17,1) = ':' ;
		AND OCCURS('T',m.cstr) = 1 ;
		AND OCCURS('-',m.cstr) = 2 ;
		AND OCCURS(':',m.cstr) = 2 ,.T.,.F. )
ENDFUNC

*-----------------------------------------------------
FUNCTION jsondatetodt( cjsondate )
*-----------------------------------------------------
	cjsondate = RTRIM(m.cjsondate,1,'Z')
	IF m.cjsondate = '0000-00-00T00:00:00'
		RETURN {}
	ELSE
		cret = EVAL('{^'+RTRIM(m.cjsondate,1,"T00:00:00")+'}')
		IF !EMPTY(m.cret)
			RETURN m.cret
		ELSE
			ERROR 'Invalid date '+cjsondate
		ENDIF
	ENDIF
ENDFUNC

******************************************
DEFINE CLASS STACK AS COLLECTION
******************************************
*---------------------------
	FUNCTION pushobject()
*---------------------------
		THIS.ADD('O')
	ENDFUNC
*---------------------------
	FUNCTION pusharray()
*---------------------------
		THIS.ADD('A')
	ENDFUNC
*--------------------------------------
	FUNCTION isobject()
*--------------------------------------
		RETURN THIS.COUNT > 0 AND THIS.ITEM( THIS.COUNT ) = 'O'
	ENDFUNC
*--------------------------------------
	FUNCTION isarray()
*--------------------------------------
		RETURN THIS.COUNT > 0 AND (THIS.ITEM( THIS.COUNT ) = 'A')
	ENDFUNC
*----------------------------
	FUNCTION POP()
*----------------------------
		cret = THIS.ITEM( THIS.COUNT )
		THIS.REMOVE( THIS.COUNT )
		RETURN m.cret
	ENDFUNC
******************************************
ENDDEFINE
******************************************
