import * as React from "react";
import TextFieldInput from "./TextFieldInput";

type State = {
  room: {
    value: string;
    error: string | undefined;
  };
  user: {
    value: string;
    error: string | undefined;
  };
};

type Field = "room" | "user";

const initialState: State = Object.freeze({
  room: {
    value: "",
    error: undefined,
  },
  user: {
    value: "",
    error: undefined,
  },
});

const validate = (field: Field) => (state: State) => {
  const value = state[field].value.trim();

  if (value === "") {
    return {
      ...state,
      [field]: {
        ...state[field],
        error: "May not be empty.",
      },
    };
  }

  return {
    ...state,
    [field]: {
      ...state[field],
      value,
      error: undefined,
    },
  };
};

function validateAll(state: State) {
  const fields: Field[] = ["room", "user"];
  return fields.reduce((oldState, field) => validate(field)(oldState), state);
}

export type Props = {
  onSubmit: (form: { room: String; user: String }) => void;
};

export default function UserRoomForm(props: Props) {
  const [state, setState] = React.useState(initialState);

  const handleChange = (field: Field) => (value: string) => {
    setState((oldState) => ({
      ...oldState,
      [field]: {
        ...oldState[field],
        value,
      },
    }));
  };

  const handleBlur = (field: Field) => () => {
    setState(validate(field));
  };

  function handleSubmit(e: { preventDefault: () => void }) {
    e.preventDefault();

    const newState = validateAll(state);

    setState(newState);

    if (newState.room.error === undefined && newState.user.error === undefined) {
      props.onSubmit({
        room: state.room.value,
        user: state.user.value,
      });
    }
  }

  function handleReset() {
    setState(initialState);
  }

  return (
    <form className="form" onSubmit={handleSubmit}>
      <TextFieldInput
        id="user-name"
        label="User"
        value={state.user.value}
        error={state.user.error}
        onBlur={handleBlur("user")}
        onChange={handleChange("user")}
      />
      <TextFieldInput
        id="room-name"
        label="Room"
        value={state.room.value}
        error={state.room.error}
        onChange={handleChange("room")}
        onBlur={handleBlur("room")}
      />
      <div className="buttons">
        <button
          className="button"
          id="join-room"
          type="submit"
          onClick={handleSubmit}
        >
          Join
        </button>
        <button
          className="button button--danger"
          id="reset-room"
          type="reset"
          onClick={handleReset}
        >
          Reset
        </button>
      </div>
    </form>
  );
}
