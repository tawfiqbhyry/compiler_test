
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftORleftANDrightNOTnonassocEQUALSNOT_EQUALSGREATERLESSGREATER_EQUALSLESS_EQUALSAND ASTERISK COMMA EQUALS FROM GREATER GREATER_EQUALS IDENTIFIER LESS LESS_EQUALS LPAREN NOT NOT_EQUALS NUMBER OR RPAREN SELECT SEMICOLON STRING WHEREquery : SELECT select_columns FROM table where_clause SEMICOLONselect_columns : ASTERISK\n                     | column_listcolumn_list : IDENTIFIER\n                  | column_list COMMA IDENTIFIERtable : IDENTIFIERwhere_clause : WHERE condition\n                   | emptycondition : condition AND condition\n                | condition OR condition\n                | NOT condition\n                | LPAREN condition RPAREN\n                | comparisoncomparison : IDENTIFIER compare_op valuecompare_op : EQUALS\n                 | NOT_EQUALS\n                 | GREATER\n                 | LESS\n                 | GREATER_EQUALS\n                 | LESS_EQUALSvalue : STRING\n             | NUMBER\n             | IDENTIFIERempty :'
    
_lr_action_items = {'SELECT':([0,],[2,]),'$end':([1,15,],[0,-1,]),'ASTERISK':([2,],[4,]),'IDENTIFIER':([2,7,8,13,17,18,21,22,25,26,27,28,29,30,31,],[6,10,11,20,20,20,20,20,35,-15,-16,-17,-18,-19,-20,]),'FROM':([3,4,5,6,11,],[7,-2,-3,-4,-5,]),'COMMA':([5,6,11,],[8,-4,-5,]),'WHERE':([9,10,],[13,-6,]),'SEMICOLON':([9,10,12,14,16,19,23,32,33,34,35,36,37,38,],[-24,-6,15,-8,-7,-13,-11,-9,-10,-12,-23,-14,-21,-22,]),'NOT':([13,17,18,21,22,],[17,17,17,17,17,]),'LPAREN':([13,17,18,21,22,],[18,18,18,18,18,]),'AND':([16,19,23,24,32,33,34,35,36,37,38,],[21,-13,-11,21,-9,21,-12,-23,-14,-21,-22,]),'OR':([16,19,23,24,32,33,34,35,36,37,38,],[22,-13,-11,22,-9,-10,-12,-23,-14,-21,-22,]),'RPAREN':([19,23,24,32,33,34,35,36,37,38,],[-13,-11,34,-9,-10,-12,-23,-14,-21,-22,]),'EQUALS':([20,],[26,]),'NOT_EQUALS':([20,],[27,]),'GREATER':([20,],[28,]),'LESS':([20,],[29,]),'GREATER_EQUALS':([20,],[30,]),'LESS_EQUALS':([20,],[31,]),'STRING':([25,26,27,28,29,30,31,],[37,-15,-16,-17,-18,-19,-20,]),'NUMBER':([25,26,27,28,29,30,31,],[38,-15,-16,-17,-18,-19,-20,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'query':([0,],[1,]),'select_columns':([2,],[3,]),'column_list':([2,],[5,]),'table':([7,],[9,]),'where_clause':([9,],[12,]),'empty':([9,],[14,]),'condition':([13,17,18,21,22,],[16,23,24,32,33,]),'comparison':([13,17,18,21,22,],[19,19,19,19,19,]),'compare_op':([20,],[25,]),'value':([25,],[36,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> query","S'",1,None,None,None),
  ('query -> SELECT select_columns FROM table where_clause SEMICOLON','query',6,'p_query','x.py',87),
  ('select_columns -> ASTERISK','select_columns',1,'p_select_columns','x.py',91),
  ('select_columns -> column_list','select_columns',1,'p_select_columns','x.py',92),
  ('column_list -> IDENTIFIER','column_list',1,'p_column_list','x.py',96),
  ('column_list -> column_list COMMA IDENTIFIER','column_list',3,'p_column_list','x.py',97),
  ('table -> IDENTIFIER','table',1,'p_table','x.py',104),
  ('where_clause -> WHERE condition','where_clause',2,'p_where_clause','x.py',108),
  ('where_clause -> empty','where_clause',1,'p_where_clause','x.py',109),
  ('condition -> condition AND condition','condition',3,'p_condition','x.py',113),
  ('condition -> condition OR condition','condition',3,'p_condition','x.py',114),
  ('condition -> NOT condition','condition',2,'p_condition','x.py',115),
  ('condition -> LPAREN condition RPAREN','condition',3,'p_condition','x.py',116),
  ('condition -> comparison','condition',1,'p_condition','x.py',117),
  ('comparison -> IDENTIFIER compare_op value','comparison',3,'p_comparison','x.py',129),
  ('compare_op -> EQUALS','compare_op',1,'p_compare_op','x.py',133),
  ('compare_op -> NOT_EQUALS','compare_op',1,'p_compare_op','x.py',134),
  ('compare_op -> GREATER','compare_op',1,'p_compare_op','x.py',135),
  ('compare_op -> LESS','compare_op',1,'p_compare_op','x.py',136),
  ('compare_op -> GREATER_EQUALS','compare_op',1,'p_compare_op','x.py',137),
  ('compare_op -> LESS_EQUALS','compare_op',1,'p_compare_op','x.py',138),
  ('value -> STRING','value',1,'p_value','x.py',142),
  ('value -> NUMBER','value',1,'p_value','x.py',143),
  ('value -> IDENTIFIER','value',1,'p_value','x.py',144),
  ('empty -> <empty>','empty',0,'p_empty','x.py',148),
]
