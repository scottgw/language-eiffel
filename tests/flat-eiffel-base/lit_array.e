class LIT_ARRAY

feature
  thing: ARRAY [STRING]
    do
      Result := <<"Hello", "World">>
    end
end

