/* Copyright (C) 2019 Nunuhara Cabbage <nunuhara@haniwa.technology>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://gnu.org/licenses/>.
 */

#ifndef GALICE_JAM_VIEW_HPP
#define GALICE_JAM_VIEW_HPP

#include <QSyntaxHighlighter>
#include <QTextDocument>
#include <QTextEdit>
#include "syntax_highlighter.hpp"

class JamView : public QTextEdit {
	Q_OBJECT
public:
	JamView(QWidget *parent = nullptr);
	JamView(const QString &text, QWidget *parent = nullptr);

private:
	SyntaxHighlighter *highlighter;
};

#endif /* GALICE_JAM_VIEW_HPP */
