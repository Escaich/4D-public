/*name: regexManager  
uid: 021EA6929399438EA838D16B58627F76 </UID>
________________________________________________________________________________
CRÉATION par  Bernard Escaich <DAT>
`•1• 04/07/2022 23:18:35  
`•2• 25/02/2023 17:17:56  Ajout fonction getDate
`•3• 20/04/2023 16:32:58  Ajout de paramètres, contrôle du start
`•4• 28/11/2024 15:12:39  Ajout getGroupNames(), adaptation de match
`•5• 30/12/2024 16:42:53  captures contien directement les propriétés
</DAT>
*/

Class constructor($pattern : Text; $textToParse : Text; $start : Integer)
	This:C1470.pattern:=(($pattern#Null:C1517) && ($pattern#"")) ? $pattern : ""
	This:C1470.textToParse:=(($textToParse#Null:C1517) && ($textToParse#"")) ? $textToParse : ""
	This:C1470.start:=(($start#Null:C1517) && ($start>=1)) ? $start : 1
	This:C1470.rest:=This:C1470.textToParse
	
	
Function getDate($format : Text)->$date : Date
/*
La date recherchée est toujours JJ MM AA, avec des séparateurs . ou /, 
Prévoir les différents formats
*/
	Case of 
		: ($format="YYYY-MM-DD")
			This:C1470.pattern:="(.*)([0-9]{4}-[0-9]{2}-[0-9]{2})(.*)"
		: ($format="DD.MM.YY")
			This:C1470.pattern:="(.*)((?:[0-9]{2}/[0-9]{2}/[0-9]{2})|(?:[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}))(.*)"
		: ($format="DD_MM_YY")
			This:C1470.pattern:="(.*)((?:[0-9]{2}/[0-9]{2}/[0-9]{2})|(?:[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}))(.*)"
		: ($format="DDMMYYYY")
			This:C1470.pattern:="(?:.*)([0-9]{8})(?:.*)"
			This:C1470.result:=This:C1470.match()
			If (This:C1470.result.isFound)
				$dateT:=This:C1470.result.content[0].text
				$day:=Num:C11(Substring:C12($dateT; 1; 2))
				$month:=Num:C11(Substring:C12($dateT; 3; 2))
				$year:=Num:C11(Substring:C12($dateT; 5; 4))
				$date:=Add to date:C393(!00-00-00!; $year; $month; $day)
				This:C1470.detectedDate:=This:C1470.content[1].text
			Else 
				Form:C1466.date:=Null:C1517
				$date:=Null:C1517
			End if 
			
		Else 
			//cas le plus général ?
			This:C1470.pattern:="(.*)((?:[0-9]{2}/[0-9]{2}/[0-9]{2})|(?:[0-9]{2}\\.[0-9]{2}\\.[0-9]{2})|(?:[0-9]{2} [0-9]{2} [0-9]{4}))(.*)"
			This:C1470.result:=This:C1470.match()
			If (This:C1470.result.isFound)
				$dateT:=This:C1470.result.content[1].text
				$day:=Num:C11(Substring:C12($dateT; 1; 2))
				$month:=Num:C11(Substring:C12($dateT; 4; 2))
				$year:=Round:C94(Year of:C25(Current date:C33); -2)+Num:C11(Substring:C12($dateT; 7; 2))
				$date:=Add to date:C393(!00-00-00!; $year; $month; $day)
			Else 
				$date:=!00-00-00!
			End if 
	End case 
/*
////Détection de trois motifs de dates, JJ/MM/AA, JJ.MM.AA et JJ MM AAAA, dans le libellé pour récupérer la date de l'opération CB
//This.regex.start:=1
//This.regex.textToParse:=$operation.fullLabel
//$operationDate:=This.regex.getDate()
//Case of 
//: (Undefined($operationDate)) || ($operationDate=!00-00-00!)
//$operation.operationDate:=$operation.$validationDate
//: ($operationDate>!00-00-00!)
//$labelBeforeDate:=This.regex.result.content[0].text
//$labelAfterDate:=This.regex.result.content[2].text
//$operation.operationDate:=$operationDate
//End case 
*/
	
	
Function _getGroupNames($patternToParse : Text)->$groups : Collection
/*
•4• 28/11/2024 15:12:39  Ajout getGroupNames(), adaptation de match()
Les noms des groupes de capture sont identifiés par ?<groupName> au début du groupe
Principe
- balayer le motif 
- à chaque parenthèse ouvrante, correspond un groupe que l'on enregistre dans une collection s'il ne commence pas par ?:
- s'il y a un nom, on l'enregistre, sinon on l'appelle groupN où N est le numéro du groupe
Motif pour chercher un nom de groupe : (?i)(?<groupName><(?:\w{1,255}>))
La procédure est récursive : match() appelle _getGroupNames() qui appelle match().
La fonction _getGroupNames() n'est pas appelée si le motif à analyser est identique au motif d'analyse, afin d'éviter une boucle infinie.
*/
	var $charNum : Integer
	var $groupNum : Integer:=1
	var $length : Integer:=Length:C16($patternToParse)
	var $pattern : Text:="(?i)(?<groupName>(?:<[a-z,A-Z,0-9]{1,255}>))"
	var $regex : cs:C1710.regexManager
	var $group:={}
	var $groups:=[]
	var $restToParse : Text:=$patternToParse
	var $start : Integer:=1
	If ($patternToParse#$pattern)
		For ($charNum; 1; $length-2)
			If ($patternToParse[[$charNum]]="(") & Not:C34((($patternToParse[[$charNum+1]]="?")=True:C214) & (($patternToParse[[$charNum+2]]=":")=True:C214))
				$group:=New object:C1471("num"; 0)
				$regex:=cs:C1710.regexManager.new()
				$regex.match($pattern; $restToParse; 1)
				If ($regex.isFound)
					$group.num:=$groupNum
					$group.name:=$regex.isFound ? Substring:C12($regex.firstText; 2; Length:C16($regex.firstText)-2) : "group"+String:C10($group.num)
					$group.nameStart:=This:C1470.start+$regex.content.first().position-1
					$group.nameEnd:=This:C1470.start+$regex.content.first().position+$regex.content.first().length-1
					$start:=$regex.isFound ? $start+$regex.content.first().position+$regex.content.first().length-1 : This:C1470.start
					$restToParse:=$regex.rest
					$groups.push($group)
					$charNum:=$group.nameEnd+1
					$groupNum:=$groupNum+1
				End if 
			End if 
		End for 
	End if 
	
	
Function match($pattern : Text; $textToParse : Text; $start : Integer; $patternNames : Collection)
/*
Les paramètres sont facultatifs mais si on veut indiquer le 2e, il faut mettre au moins une chaine vide pour le premier.
La valeur passée en paramètre s'impose par rapport à celle de la propriété.
Pour faciliter les tests avec un Case of, une chaine vide est retournée en cas d'échec de la recherche.
A remplacer à terme par match4()
*/
	This:C1470.pattern:=(($pattern#Null:C1517) && ($pattern#"")) ? $pattern : This:C1470.pattern
	This:C1470.textToParse:=(($textToParse#Null:C1517) && ($textToParse#"")) ? $textToParse : This:C1470.textToParse
	This:C1470.start:=(($start#Null:C1517) && ($start>=1)) ? $start : This:C1470.start
	ARRAY LONGINT:C221($lengths_; 0)
	ARRAY LONGINT:C221($positions_; 0)
	var $result : Object
	$result:=New object:C1471
	This:C1470.content:=New collection:C1472
	This:C1470.start:=(OB Is defined:C1231(This:C1470; "start") ? ((This:C1470.start>=1) ? This:C1470.start : 1) : 1)
	This:C1470.isFound:=Match regex:C1019(This:C1470.pattern; This:C1470.textToParse; This:C1470.start; $positions_; $lengths_)
	If (This:C1470.isFound)
		$nb_l:=Size of array:C274($positions_)
		For ($i; 1; Size of array:C274($positions_))
			$result:=New object:C1471
			$result.position:=$positions_{$i}
			$result.length:=$lengths_{$i}
			$result.text:=Substring:C12(This:C1470.textToParse; $positions_{$i}; $lengths_{$i})
			This:C1470.rest:=Substring:C12(This:C1470.textToParse; $positions_{Size of array:C274($positions_)}+$lengths_{Size of array:C274($positions_)})
			This:C1470.content.push($result)
		End for 
		This:C1470.firstText:=This:C1470.content[0].text
		This:C1470.end:=$positions_{$nb_l}+$lengths_{$nb_l}
	Else 
		//This.content.push(New object("text"; ""))
		//This.firstText:=""
		//This.rest:=This.textToParse
	End if 
	$0:=This:C1470
	
	
/*
Function match2($pattern : Object; $textToParse : Text; $start : Integer; $groups : Collection)->$output : Object
/*
Les paramètres sont facultatifs mais si on veut indiquer le 2e, il faut mettre au moins une chaine vide pour le premier.
La valeur passée en paramètre s'impose par rapport à celle de la propriété.
Pour faciliter les tests avec un Case of, un objet vide est retourné en cas d'échec de la recherche.
•4• 28/11/2024 15:12:39  
La V2 introduit :
- un nommage des captures, plus faciles à utiliser
- une nouvelle définition des radicaux en 2 éléments
Le pattern objet comporte le motif pattern.text et le nom des groupes extrait du pattern ou groupN pour les groupes sans nom.
*/
This.pattern:=($pattern#Null) ? $pattern : This.pattern
This.textToParse:=(($textToParse#Null) && ($textToParse#"")) ? $textToParse : This.textToParse
This.start:=(($start#Null) && ($start>=1)) ? $start : This.start
	
var $result : Object
ARRAY LONGINT($lengths_; 0)
ARRAY LONGINT($positions_; 0)
$result:=New object
This.content:=New collection
This.start:=(OB Is defined(This; "start") ? ((This.start>=1) ? This.start : 1) : 1)
This.isFound:=Match regex(This.pattern.text; This.textToParse; This.start; $positions_; $lengths_)
If (This.isFound)
$nb_l:=Size of array($positions_)
For ($i; 1; Size of array($positions_))
$result:=New object
$result.position:=$positions_{$i}
$result.length:=$lengths_{$i}
$result.text:=Substring(This.textToParse; $positions_{$i}; $lengths_{$i})
This.rest:=Substring(This.textToParse; $positions_{Size of array($positions_)}+$lengths_{Size of array($positions_)})
This.content.push($result)
End for 
This.firstText:=This.content[0].text
This.end:=$positions_{$nb_l}+$lengths_{$nb_l}
Else 
This.content.push(New object("text"; ""))
This.firstText:=""
End if 
If (Count parameters<=3)
$0:=This.result.names
Else   // Les captures sont nommées
	
	
End if 
*/
	
	
Function match3($pattern : Text; $textToParse : Text; $start : Integer; $groups : Collection)
/*
Les paramètres sont facultatifs mais si on veut indiquer le 2e, il faut mettre au moins une chaine vide pour le premier.
La valeur passée en paramètre s'impose par rapport à celle de la propriété.
Pour faciliter les tests avec un Case of, une chaine vide est retournée en cas d'échec de la recherche.
*/
	This:C1470.pattern:=(($pattern#Null:C1517) && ($pattern#"")) ? $pattern : This:C1470.pattern
	This:C1470.textToParse:=(($textToParse#Null:C1517) && ($textToParse#"")) ? $textToParse : This:C1470.textToParse
	This:C1470.start:=(($start#Null:C1517) && ($start>=1)) ? $start : This:C1470.start
	This:C1470.groups:=(($groups#Null:C1517) && ($groups#"")) ? $groups : This:C1470.groups
	This:C1470.captures:=New collection:C1472
	ARRAY LONGINT:C221($lengths_; 0)
	ARRAY LONGINT:C221($positions_; 0)
	var $result : Object
	
	$result:=New object:C1471
	This:C1470.content:=New object:C1471
	This:C1470.start:=1  //(OB Is defined(This; "start") ? ((This.start>=1) ? This.start : 1) : 1)
	This:C1470.isFound:=Match regex:C1019(This:C1470.pattern; This:C1470.textToParse; This:C1470.start; $positions_; $lengths_)
	If (This:C1470.isFound)
		$nb_l:=Size of array:C274($positions_)
		For ($i; 1; Size of array:C274($positions_))
			This:C1470.groups[$i-1].value:=Substring:C12(This:C1470.textToParse; $positions_{$i}; $lengths_{$i})
			$result[This:C1470.groups[$i-1].name]:=New object:C1471("property"; This:C1470.groups[$i-1].name; "value"; This:C1470.groups[$i-1].value; "position"; $positions_{$i}; "length"; $lengths_{$i})
			This:C1470.captures.push(New object:C1471($result[This:C1470.groups[$i-1].name].property; $result[This:C1470.groups[$i-1].name].value))
			This:C1470.rest:=Substring:C12(This:C1470.textToParse; $positions_{Size of array:C274($positions_)}+$lengths_{Size of array:C274($positions_)})
		End for 
		This:C1470.end:=$positions_{$nb_l}+$lengths_{$nb_l}
	Else 
		This:C1470.content.push(New object:C1471("text"; ""))
		This:C1470.firstText:=""
		This:C1470.rest:=This:C1470.textToParse
	End if 
	This:C1470.content:=$result
	$0:=This:C1470
	
	
Function match4($pattern : Text; $textToParse : Text; $start : Integer; $groups : Collection)
/*
•4• 28/11/2024 15:12:39  Ajout getGroupNames(), adaptation de match()
	
Les paramètres sont facultatifs mais si on veut indiquer le 2e, il faut mettre au moins une chaine vide pour le premier.
La valeur passée en paramètre s'impose par rapport à celle de la propriété.
Pour faciliter les tests avec un Case of, une chaine vide est retournée en cas d'échec de la recherche.
*/
	This:C1470.pattern:=(($pattern#Null:C1517) && ($pattern#"")) ? $pattern : This:C1470.pattern
	This:C1470.textToParse:=(($textToParse#Null:C1517) && ($textToParse#"")) ? $textToParse : This:C1470.textToParse
	This:C1470.start:=(($start#Null:C1517) && ($start>=1)) ? $start : This:C1470.start
	This:C1470.groups:=New collection:C1472
	This:C1470.captures:=New collection:C1472
	
	This:C1470.groups:=This:C1470._getGroupNames(This:C1470.pattern)
	
	ARRAY LONGINT:C221($lengths_; 0)
	ARRAY LONGINT:C221($positions_; 0)
	var $result : Object
	$result:=New object:C1471
	This:C1470.content:=New object:C1471
	This:C1470.start:=1  // 
	This:C1470.isFound:=Match regex:C1019(This:C1470.pattern; This:C1470.textToParse; This:C1470.start; $positions_; $lengths_)
	If (This:C1470.isFound)
		$nb_l:=Size of array:C274($positions_)
		For ($i; 1; Size of array:C274($positions_))
			This:C1470.groups[$i-1].value:=Substring:C12(This:C1470.textToParse; $positions_{$i}; $lengths_{$i})
			$result[This:C1470.groups[$i-1].name]:=New object:C1471("property"; This:C1470.groups[$i-1].name; "value"; This:C1470.groups[$i-1].value; "position"; $positions_{$i}; "length"; $lengths_{$i})
			This:C1470.captures.push(New object:C1471($result[This:C1470.groups[$i-1].name].property; $result[This:C1470.groups[$i-1].name].value))
			This:C1470.rest:=Substring:C12(This:C1470.textToParse; $positions_{Size of array:C274($positions_)}+$lengths_{Size of array:C274($positions_)})
		End for 
		This:C1470.end:=$positions_{$nb_l}+$lengths_{$nb_l}
	End if 
	This:C1470.content:=$result
	$0:=This:C1470
	
	
Function match5($pattern : Text; $textToParse : Text; $start : Integer; $groups : Collection)
/*
•4• 28/11/2024 15:12:39  Ajout getGroupNames(), adaptation de match()
•5• 30/12/2024 16:42:53  captures contien directement les propriétés
	
Les paramètres sont facultatifs mais si on veut indiquer le 2e, il faut mettre au moins une chaine vide pour le premier.
La valeur passée en paramètre s'impose par rapport à celle de la propriété.
Pour faciliter les tests avec un Case of, une chaine vide est retournée en cas d'échec de la recherche.
Le résultat est retourné sous plusieurs formes selon l'utilisation que l'on veut en faire :
- le content est la liste des propriétés capturées ; si elles ont un nom significatif, elles sont directement exploitables, sinon, elles ont un nom générique groupN
- les groupes ont pluis d'information, comme le début et la fin de la chaîne capturée
- isFound contient un booléen indiquant le succès.
*/
	This:C1470.pattern:=(($pattern#Null:C1517) && ($pattern#"")) ? $pattern : This:C1470.pattern
	This:C1470.textToParse:=(($textToParse#Null:C1517) && ($textToParse#"")) ? $textToParse : This:C1470.textToParse
	This:C1470.start:=(($start#Null:C1517) && ($start>=1)) ? $start : This:C1470.start
	//--
	This:C1470.groups:=This:C1470._getGroupNames(This:C1470.pattern)
	//--
	This:C1470.captures:=New object:C1471
	ARRAY LONGINT:C221($lengths_; 0)
	ARRAY LONGINT:C221($positions_; 0)
	var $result : Object
	$result:=New object:C1471
	This:C1470.content:=New object:C1471
	This:C1470.start:=1  // 
	This:C1470.isFound:=Match regex:C1019(This:C1470.pattern; This:C1470.textToParse; This:C1470.start; $positions_; $lengths_)
	If (This:C1470.isFound)
		$nb_l:=Size of array:C274($positions_)
		For ($i; 1; Size of array:C274($positions_))
			This:C1470.groups[$i-1].value:=Substring:C12(This:C1470.textToParse; $positions_{$i}; $lengths_{$i})
			$result[This:C1470.groups[$i-1].name]:=New object:C1471("property"; This:C1470.groups[$i-1].name; "value"; This:C1470.groups[$i-1].value; "position"; $positions_{$i}; "length"; $lengths_{$i})
			This:C1470.captures[$result[This:C1470.groups[$i-1].name].property]:=$result[This:C1470.groups[$i-1].name].value
			This:C1470.rest:=Substring:C12(This:C1470.textToParse; $positions_{Size of array:C274($positions_)}+$lengths_{Size of array:C274($positions_)})
		End for 
		This:C1470.end:=$positions_{$nb_l}+$lengths_{$nb_l}
	End if 
	This:C1470.content:=$result
	$0:=This:C1470
	
	
Function matchAll($pattern : Variant; $textToParse : Text; $start : Integer; $groups : Collection)
/*
Cette fonction sort une liste complète de toutes les analyses avec le motif
*/
	This:C1470.pattern:=(($pattern#Null:C1517) && ($pattern#"")) ? $pattern : This:C1470.pattern
	This:C1470.textToParse:=(($textToParse#Null:C1517) && ($textToParse#"")) ? $textToParse : This:C1470.textToParse
	This:C1470.start:=($start>0) ? $start : 1
	This:C1470.groups:=(($groups#Null:C1517) && ($groups#"")) ? $groups : This:C1470.groups
	ARRAY LONGINT:C221($lengths_; 0)
	ARRAY LONGINT:C221($positions_; 0)
	This:C1470.contents:=New collection:C1472
	Repeat 
		$pattern:=This:C1470.pattern
		$textToParse:=This:C1470.textToParse
		$start:=This:C1470.start
		$groups:=This:C1470.groups
		$result:=This:C1470.match($pattern; $textToParse; $start)  //; This.groups)
		If ($result.isFound=False:C215)
			break
		Else 
			//This.start:=$result.content[1].position+$result.content[1].length
			This:C1470.start:=$result.content[0].position+$result.content[0].length
		End if 
		This:C1470.contents.push(This:C1470.content)
	Until (False:C215)
	$0:=This:C1470
	
	
Function matchAll2($pattern : Variant; $textToParse : Text; $start : Integer; $groups : Collection)
/*
Cette fonction sort une liste complète de toutes les analyses avec le motif
*/
	This:C1470.pattern:=(($pattern#Null:C1517) && ($pattern#"")) ? $pattern : This:C1470.pattern
	This:C1470.textToParse:=(($textToParse#Null:C1517) && ($textToParse#"")) ? $textToParse : This:C1470.textToParse
	This:C1470.start:=($start>0) ? $start : 1
	This:C1470.groups:=(($groups#Null:C1517) && ($groups#"")) ? $groups : This:C1470.groups
	ARRAY LONGINT:C221($lengths_; 0)
	ARRAY LONGINT:C221($positions_; 0)
	This:C1470.contents:=New collection:C1472
	Repeat 
		$pattern:=This:C1470.pattern
		$textToParse:=This:C1470.textToParse
		$start:=This:C1470.start
		$groups:=This:C1470.groups
		$result:=This:C1470.match3($pattern; $textToParse; $start)  //; This.groups)
		If ($result.isFound=False:C215)
			break
		Else 
			//This.start:=$result.content[1].position+$result.content[1].length
			This:C1470.start:=$result.content[0].position+$result.content[0].length
		End if 
		This:C1470.contents.push(This:C1470.content)
	Until (False:C215)
	$0:=This:C1470
	
	
	
	//MARK:- Test
Function autotest($parameters : Object)->$result : Object
	
	