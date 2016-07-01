structure Organisms : POSET =
struct
  datatype t =
    ANIMAL | MAMMAL | MARINE | LLAMA | DOLPHIN | JELLYFISH
    | VEGETABLE | CARROT

  fun R (MAMMAL,       ANIMAL) = true
    | R (MARINE,       ANIMAL) = true
    | R (DOLPHIN,      MAMMAL) = true
    | R (DOLPHIN,      MARINE) = true
    | R (LLAMA,        MAMMAL) = true
    | R (JELLYFISH,    MARINE) = true
    | R (CARROT,    VEGEATBLE) = true
    | R (_,                 _) = false

end
