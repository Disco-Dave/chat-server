import * as React from "react";

export type Props = { title: String; children?: React.ReactNode };

export default function Layout(props: Props) {
  return (
    <main className="layout">
      <h1 className="layout__title">{props.title}</h1>
      <div className="layout__body">{props.children}</div>
    </main>
  );
}
