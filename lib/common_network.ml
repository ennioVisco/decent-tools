open Utils

type coordinate = int
type coordinates = coordinate * coordinate

let stringrep_coordinate (c:coordinate) : string = string_of_int c
let stringrep_coordinates (c:coordinates) : string = stringrep_intint c
