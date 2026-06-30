package example.utils;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;

public class SampleGui {
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Swing JTree Example");
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setSize(350, 300);

            // 1. Create the Root node
            DefaultMutableTreeNode root = new DefaultMutableTreeNode("Software");

            // 2. Create Branch nodes
            DefaultMutableTreeNode programming = new DefaultMutableTreeNode("Programming Languages");
            DefaultMutableTreeNode databases = new DefaultMutableTreeNode("Databases");

            // 3. Create Leaf nodes and attach them to branches
            programming.add(new DefaultMutableTreeNode("Java"));
            programming.add(new DefaultMutableTreeNode("Python"));
            programming.add(new DefaultMutableTreeNode("C++"));

            databases.add(new DefaultMutableTreeNode("MySQL"));
            databases.add(new DefaultMutableTreeNode("PostgreSQL"));

            // 4. Attach branches to the Root
            root.add(programming);
            root.add(databases);

            // 5. Initialize the JTree
            JTree tree = new JTree(root);

            // Optional: Customize selection mode
            tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

            // Listen for node selection
            tree.addTreeSelectionListener(e -> {
                DefaultMutableTreeNode selectedNode = (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();
                if (selectedNode != null) {
                    System.out.println("Selected: " + selectedNode.getUserObject().toString());
                }
            });

            // 6. Put the tree in a ScrollPane and add to frame
            JScrollPane scrollPane = new JScrollPane(tree);
            frame.add(scrollPane, BorderLayout.CENTER);

            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }
}
