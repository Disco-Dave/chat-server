export type Props = {
  id: string;
  label: string;
  value: string;
  onChange: (newValue: string) => void;
  onBlur: () => void;
  error?: string;
};

export default function TextFieldInput(props: Props) {
  const fieldClassName = props.error ? "field field--invalid" : "field";

  return (
    <div className={fieldClassName}>
      <label className="field__label" htmlFor={props.id}>
        {props.label}
      </label>
      <input
        className="field__input"
        id={props.id}
        name={props.id}
        type="text"
        value={props.value}
        onBlur={props.onBlur}
        onChange={({ target: { value } }) => props.onChange(value)}
      />

      { props.error && (
        <p className="field__feedback">{props.error}</p>
      )}
    </div>
  );
}
