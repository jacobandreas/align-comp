/* Copyright (C) Kais Dukes.
 * Email: kais@kaisdukes.com
 *
 * This file is part of Train Robots.
 *
 * This is free software: you can redistribute it and/or modify it under the terms
 * of the GNU General Public License as published by the Free Software Foundation,
 * either version 3 of the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Train Robots. If not, see <http://www.gnu.org/licenses/>.
 */

package semeval2014task6;

import java.util.List;

import org.junit.Test;

import com.trainrobots.core.DataContext;
import com.trainrobots.core.corpus.Command;
import com.trainrobots.core.corpus.Corpus;
import com.trainrobots.core.nodes.Node;
import com.trainrobots.core.rcl.ActionAttribute;
import com.trainrobots.core.rcl.CardinalAttribute;
import com.trainrobots.core.rcl.ColorAttribute;
import com.trainrobots.core.rcl.Entity;
import com.trainrobots.core.rcl.IndicatorAttribute;
import com.trainrobots.core.rcl.OrdinalAttribute;
import com.trainrobots.core.rcl.Rcl;
import com.trainrobots.core.rcl.RclVisitor;
import com.trainrobots.core.rcl.RelationAttribute;
import com.trainrobots.core.rcl.TypeAttribute;
import com.trainrobots.nlp.csp.Planner;
import com.trainrobots.nlp.scenes.SceneManager;
import com.trainrobots.nlp.scenes.WorldEntity;
import com.trainrobots.nlp.scenes.WorldModel;
import com.trainrobots.nlp.tokenizer.Tokenizer;

public class Example {

	@Test
	public void Example1_getTreesWithAlignmentInfo() {

		// ============================================================
		// This example gets trees from the treebank with alignment
		// annotation. See Example 2 for how to remove this extra data.
		// ============================================================

		// Set path to treebank.
		DataContext.setDataPath("trial_data");

		// Display the first 5 trees in the treebank.
		int i = 0;
		for (Command command : Corpus.getCommands()) {
			if (command.rcl == null) { // Skip unannotated commands.
				continue;
			}
			if (++i > 5) {
				return;
			}
			System.out.println(command.id);
			System.out.println(command.text);
			System.out.println(command.rcl);
			System.out.println();
		}
	}

	@Test
	public void Exmaple2_getTreesWithoutAlignmentInfo() {

		// ============================================================
		// This example gets trees from the treebank without alignment
		// annotation.
		// ============================================================

		// Set path to treebank.
		DataContext.setDataPath("trial_data");

		// Display the first 5 trees in the treebank.
		int i = 0;
		for (Command command : Corpus.getCommands()) {
			if (command.rcl == null) { // Skip unannotated commands.
				continue;
			}
			if (++i > 5) {
				return;
			}
			System.out.println(command.id);
			System.out.println(command.text);
			Node node = command.rcl.toNode();
			removeAlignment(node);
			System.out.println(node);
			System.out.println();
		}
	}

	@Test
	public void Example3_getAlignedTokens() {

		// ============================================================
		// This example gets the annotated mappings between tokens
		// and RCL leaves in the treebank.
		// ============================================================

		// Set path to treebank.
		DataContext.setDataPath("trial_data");

		// Proces all commands in the treebank.
		for (Command command : Corpus.getCommands()) {
			if (command.rcl == null) { // Skip unannotated commands.
				continue;
			}

			// Tokenize the NL text.
			List<Node> tokens = Tokenizer.getTokens(command.text).children;

			// Map tokens to aligned RCL leaves.
			mapTokens(tokens, command.rcl);
		}
	}

	@Test
	public void Example4_useSpatialContext() {

		// ============================================================
		// This example demonstrates how to check if an RCL subtree
		// is valid for the shapes and board configuration associated
		// with a command. The key fact here is that each command is
		// annotated witin the context of a scene. There are 1000
		// possible different scenes in the treebank for the various
		// commands (each command has one of these scene numbers).
		// Using the scene number, we can check if an RCL fragment is
		// valid.
		// ============================================================

		// Set path to treebank.
		DataContext.setDataPath("trial_data");

		// Get command with ID 34 from the treebank, and display the command.
		Command command = Corpus.getCommand(34);
		System.out.println(command.id);
		System.out.println(command.text);
		Node node = command.rcl.toNode();
		removeAlignment(node);
		System.out.println(node);
		System.out.println();

		// Every command is annotated in the context of a world model. This
		// represents the board (the shapes, their colors and positions), that
		// were shown to annotators when asked to annotate this command.
		WorldModel world = SceneManager.getScene(command.sceneNumber).before;

		// The planner will try to map objects from RCL onto shapes.
		Planner planner = new Planner(world);

		// Check if the RCL fragment for 'blue cube' makes sense for this scene.
		{
			Entity entity = Entity
					.fromString("(entity: (type: cube) (color: blue))");
			List<WorldEntity> groundings = planner.getGroundings(entity);
			System.out.println("MAPPED: " + groundings);
		}

		// Now check if 'red cube' makes sense. There are multiple of these on
		// the board.
		{
			Entity entity = Entity
					.fromString("(entity: (type: cube) (color: red))");
			List<WorldEntity> groundings = planner.getGroundings(entity);
			System.out.println("MAPPED: " + groundings);
		}

		// But there are no purple prisms...
		{
			Entity entity = Entity
					.fromString("(entity: (type: prism) (color: magenta))");
			List<WorldEntity> groundings = planner.getGroundings(entity);
			System.out.println("MAPPED: " + groundings);
		}
	}

	private void mapTokens(final List<Node> tokens, Rcl rcl) {

		rcl.recurse(new RclVisitor() {
			public void visit(Rcl parent, ActionAttribute attribute) {
				process(attribute);
			}

			public void visit(Rcl parent, ColorAttribute attribute) {
				process(attribute);
			}

			public void visit(Rcl parent, IndicatorAttribute attribute) {
				process(attribute);
			}

			public void visit(Rcl parent, RelationAttribute attribute) {
				process(attribute);
			}

			public void visit(Rcl parent, TypeAttribute attribute) {
				process(attribute);
			}

			public void visit(Rcl parent, OrdinalAttribute attribute) {
				process(attribute);
			}

			public void visit(Rcl parent, CardinalAttribute attribute) {
				process(attribute);
			}

			private void process(Rcl rcl) {

				// Does this RCL node have alignment info?
				int start = rcl.tokenStart();
				int end = rcl.tokenEnd();
				if (start == 0) {
					return;
				}

				// Print out the alignment.
				Node leaf = rcl.toNode();
				removeAlignment(leaf);
				System.out.print(leaf + " -->");
				for (int i = start; i <= end; i++) {
					System.out.print(" [" + tokens.get(i - 1).getValue() + "]");
				}
				System.out.println();
			}
		});
	}

	private static void removeAlignment(Node node) {
		if (node.children != null) {
			for (int i = node.children.size() - 1; i >= 0; i--) {
				Node child = node.children.get(i);
				if (child.tag.equals("token:")) {
					node.children.remove(i);
				} else {
					removeAlignment(child);
				}
			}
		}
	}
}