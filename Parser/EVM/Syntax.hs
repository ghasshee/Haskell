module Syntax where 

import Data.Word 
import Bits


data Undefined = Undefined 

data WorldState = Map (Address -> Account) 

type Address = Undefined -- 160-bit identifier

data Account = Account Nonce Balance StorageRoot CodeHash 





type Nonce          = Uint -- Number of transactions sent from this address 
type Balance        = Uint 
type StorageRoot    = Undefined -- 256-bit hash of the root node of a Merkle Patricia Tree 
type CodeHash       = Undefined -- hashed code 






data ByteCode   = 
                  Ox00 | Ox01 | Ox02 | Ox03 | Ox04 | Ox05 | Ox06 | Ox07 | Ox08 | Ox09 | Ox0a | Ox0b                             
                | Ox10 | Ox11 | Ox12 | Ox13 | Ox14 | Ox15 | Ox16 | Ox17 | Ox18 | Ox19 | Ox1a                                    
                | Ox20                                                                                                          
                | Ox30 | Ox31 | Ox32 | Ox33 | Ox34 | Ox35 | Ox36 | Ox37 | Ox38 | Ox39 | Ox3a | Ox3b | Ox3c | Ox3d | Ox3e        
                | Ox40 | Ox41 | Ox42 | Ox43 | Ox44 | Ox45                                                                       
                | Ox50 | Ox51 | Ox52 | Ox53 | Ox54 | Ox55 | Ox56 | Ox57 | Ox58 | Ox59 | Ox5a | Ox5b                             
                | Ox60 | Ox61 | Ox62 | Ox63 | Ox64 | Ox65 | Ox66 | Ox67 | Ox68 | Ox69 | Ox6a | Ox6b | Ox6c | Ox6d | Ox6e | Ox6f 
                | Ox70 | Ox71 | Ox72 | Ox73 | Ox74 | Ox75 | Ox76 | Ox77 | Ox78 | Ox79 | Ox7a | Ox7b | Ox7c | Ox7d | Ox7e | Ox7f 
                | Ox80 | Ox81 | Ox82 | Ox83 | Ox84 | Ox85 | Ox86 | Ox87 | Ox88 | Ox89 | Ox8a | Ox8b | Ox8c | Ox8d | Ox8e | Ox8f 
                | Ox90 | Ox91 | Ox92 | Ox93 | Ox94 | Ox95 | Ox96 | Ox97 | Ox98 | Ox99 | Ox9a | Ox9b | Ox9c | Ox9d | Ox9e | Ox9f 
                | Oxa0 | Oxa1 | Oxa2 | Oxa3 | Oxa4                                                                              
                | Oxf0 | Oxf1 | Oxf2 | Oxf3 | Oxf4                                    | Oxfa               | Oxfd | Oxfe | Oxff 

data Opcode     = 
                  STOP | ADD | MUL | SUB | DIV | SDIV | MOD | SMOD | ADDMOD | MULMOD | EXP | SIGNEXTEND
                | LT   | GT   | SLT  | SGT  | EQ | ISZERO | AND  | OR   | XOR  | NOT  | BYTE  
                | SHA3 
                | ADDRESS | BALANCE | ORIGIN | CALLER | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY 
                        | CODESIZE | GASPRICE | EXTCODESIZE | EXTCODECOPY | RETURNDATASIZE | RETURNDATACOPY
                | BLOCKHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT 
                | POP | MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE | JUMP | JUMPI | PC | MSIZE | GAS | JUMDEST 
                |PUSH1 |PUSH2 |PUSH3 |PUSH4 |PUSH5 |PUSH6 |PUSH7 |PUSH8 |PUSH9 |PUSH10|PUSH11|PUSH12|PUSH13|PUSH14|PUSH15|PUSH16
                |PUSH17|PUSH18|PUSH19|PUSH20|PUSH21|PUSH22|PUSH23|PUSH24|PUSH25|PUSH26|PUSH27|PUSH28|PUSH29|PUSH30|PUSH31|PUSH32
                | DUP1 | DUP2 | DUP3 | DUP4 | DUP5 | DUP6 | DUP7 | DUP8 | DUP9 | DUP10| DUP11| DUP12| DUP13| DUP14| DUP15| DUP16
                |SWAP1 |SWAP2 |SWAP3 |SWAP4 |SWAP5 |SWAP6 |SWAP7 |SWAP8 |SWAP9 |SWAP10|SWAP11|SWAP12|SWAP13|SWAP14|SWAP15|SWAP16
                | LOG0 | LOG1 | LOG2 | LOG3 | LOG4 
                | CREATE | CALL | CALLCODE | RETURN | DELEGATECALL | STATICCALL | REVERT | INVALID | SELFDESTRUCT 







