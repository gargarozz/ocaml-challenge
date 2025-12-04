type person = {
  name : string;
  age : int;
  email : string option;
}

let make_person name age email = if age < 0 || age > 150 then failwith "unbound age" else {name = name; age = age; email = email};;

let get_email p = p.email;;

let can_send_adult_email p = (p.age >= 18) && (p.email != None);;

let () = 
  let gino = make_person "gino" 21 (Some "gino@gmail.com") in
  let pino = make_person "pino" 23 None in
  let anna = make_person "anna" 16 (Some "anna@tiscali.it") in

  assert(get_email gino = Some "gino@gmail.com");
  assert(get_email pino = None);
  assert(can_send_adult_email gino);
  assert(not (can_send_adult_email pino));
  assert(not (can_send_adult_email anna));;
