structure Organisms :> REFINEMENT_SET =
struct
  datatype t =
    ANIMAL | MAMMAL | MARINE | LLAMA | DOLPHIN | JELLYFISH
    | VEGETABLE | CARROT

  fun eq (x, y) = x = y

  fun R (MAMMAL, ANIMAL) = true
    | R (MARINE, ANIMAL) = true
    | R (DOLPHIN, MAMMAL) = true
    | R (DOLPHIN, MARINE) = true
    | R (LLAMA, MAMMAL) = true
    | R (JELLYFISH, MARINE) = true
    | R (CARROT, VEGEATBLE) = true
    | R (_, _) = false

  fun toString ANIMAL = "animal"
    | toString MAMMAL = "mammal"
    | toString MARINE = "marine"
    | toString LLAMA = "llama"
    | toString DOLPHIN = "dolphin"
    | toString JELLYFISH = "jellyfish"
    | toString VEGETABLE = "vegetable"
    | toString CARROT = "carrot"

  exception ReadError

  fun read "animal"    = ANIMAL
    | read "mammal"    = MAMMAL
    | read "marine"    = MARINE
    | read "llama"     = LLAMA
    | read "dolphin"   = DOLPHIN
    | read "jellyfish" = JELLYFISH
    | read "vegetable" = VEGETABLE
    | read "carrot"    = CARROT
    | read _           = raise ReadError

end
