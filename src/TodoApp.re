type item = {
  id: int,
  title: string,
  completed: bool
};

type state = {
  /* This is a type w/ a type argument,
   * similar to List<Item> in TypeScript,
   * Flow, or Java */
  items: list(item)
};

type action =
  | AddItem(string)
  | ToggleItem(int);

/* Shortened name for converting strings to elements */
let str = ReasonReact.stringToElement;

/*  stateless component, except this one accepts a property (Item) */
module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  /* ~argname syntax means this function takes a labeled argument
   * which is known as item both externally and internally */
  let make = (~item, ~onToggle, _children) => {
    ...component,
    render: (_) =>
      <div className="item" onClick=((_evt) => onToggle())>
        <input
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean(item.completed))
        />
        (str(item.title))
      </div>
  };
};

/* convert it into a "catch-all javascript object",
 * and get out the value with the "magic accessor syntax" ##value.*/
let valueFromEvent = (evt) : string => (
  evt
  |> ReactEventRe.Form.target
  |> ReactDOMRe.domElementToObj
)##value;

module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _text) => ReasonReact.Update(newText),
    render: ({state: text, reduce}) =>
      <input
        value=text
        _type="text"
        placeholder="Write something to do"
        onChange=(reduce((evt) => valueFromEvent(evt)))
        onKeyDown=((evt) =>
          if (ReactEventRe.Keyboard.key(evt) == "Enter") {
            onSubmit(text);
            (reduce(() => ""))()
          }
        )
      />
  };
};

/* Make a reducer component */
let component = ReasonReact.reducerComponent("TodoApp");

let lastId = ref(0);
let newItem = (text) => {
  /* You update a ref with := */
  /* to access the value you dereference it with ^ */
  /* now we can add our key property to the */
  lastId := lastId^ + 1;
  {id: lastId^, title: text, completed: false}
};

let make = (_children) => {
  ...component,
  /* initialState is what you'd expect */
  initialState: () => {
    /* use the Id to use as a key */
    items: [{
      id: 0,
      title: "Write some things to do",
      completed: false
    }]
  },
  /* Kinda like redux
   * describes the ways that our state can be updated */
  reducer: (action, {items}) =>
    switch action {
    | AddItem(text) => ReasonReact.Update({items: [newItem(text), ...items]})
    | ToggleItem(id) =>
      let items =
        List.map(
          (item) =>
            item.id === id ?
              {...item, completed: ! item.completed} : item,
          items
        );
      ReasonReact.Update({items: items})
    },
  /* Reacting to events and changing state */
  render: ({state: {items}, reduce}) => {
    let numItems = List.length(items);
    <div className="app">
      <div className="title">
        (str("What to do"))
        /* Transform the event argument into an action */
        <Input onSubmit=(reduce((text) => AddItem(text))) />
      </div>
      <div className="items">
        (
          ReasonReact.arrayToElement(Array.of_list(List.map(
            (item) =>
              <TodoItem
                key=(string_of_int(item.id))
                onToggle=(reduce(() => ToggleItem(item.id)))
                item
              />,
            items
          )))
        )
      </div>
      <div className="footer">
        (str(string_of_int(numItems) ++ " items"))
      </div>
    </div>
  }
};
