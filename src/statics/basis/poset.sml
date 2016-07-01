structure Organisms : POSET =
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

end
